package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"strings"
	"sync"
	"text/template"
	"time"

	"github.com/BurntSushi/xgb"
	"github.com/BurntSushi/xgb/randr"
	"github.com/BurntSushi/xgb/xproto"
	cl "github.com/go-pa/colorlab"
	process "github.com/shirou/gopsutil/v3/process"
)

// Screen .
type Screen struct {
	X int16
	Y int16
	W uint16
	H uint16
}

type Screens []Screen

var (
	solarized = cl.Solarized{
		Base: cl.Base{
			Base03: cl.SolarizedBase03,
			Base02: cl.SolarizedBase02,
			Base01: cl.SolarizedBase01,
			Base00: cl.SolarizedBase00,
			Base0:  cl.SolarizedBase0,
			Base1:  cl.SolarizedBase1,
			Base2:  cl.SolarizedBase2,
			Base3:  cl.SolarizedBase3,
		},
		Accents: cl.Accents{
			Blue:    cl.SolarizedBlue,
			Cyan:    cl.SolarizedCyan,
			Green:   cl.SolarizedGreen,
			Magenta: cl.SolarizedMagenta,
			Orange:  cl.SolarizedOrange,
			Red:     cl.SolarizedRed,
			Violet:  cl.SolarizedViolet,
			Yellow:  cl.SolarizedYellow,
		},
	}
	// solarizedDarkHighContrast = cl.Solarized{
	// 	Base:    solarized.Base.Clone().ChangeLightness(0.04, -0.02),
	// 	Accents: solarized.Accents.Clone().ChangeLightness(0.05),
	// }
	// solarizedLightHighContrast = cl.Solarized{
	// 	Base:    solarized.Base.Clone().ChangeLightness(0.02, -0.05),
	// 	Accents: solarized.Accents.Clone().ChangeLightness(-0.05),
	// }
)

func getScreens() (Screens, error) {

	X, _ := xgb.NewConn()

	// Every extension must be initialized before it can be used.
	err := randr.Init(X)
	if err != nil {
		return nil, err
	}

	// Get the root window on the default screen.
	root := xproto.Setup(X).DefaultScreen(X).Root

	// Gets the current screen resources. Screen resources contains a list
	// of names, crtcs, outputs and modes, among other things.
	resources, err := randr.GetScreenResources(X, root).Reply()
	if err != nil {
		return nil, err

	}

	// // Iterate through all of the outputs and show some of their info.
	// for _, output := range resources.Outputs {
	// 	info, err := randr.GetOutputInfo(X, output, 0).Reply()
	// 	if err != nil {
	// 		log.Fatal(err)
	// 	}

	// 	if info.Connection == randr.ConnectionConnected {
	// 		bestMode := info.Modes[0]
	// 		for _, mode := range resources.Modes {
	// 			if mode.Id == uint32(bestMode) {
	// 				fmt.Printf("Width: %d, Height: %d\n",
	// 					mode.Width, mode.Height)

	// 			}
	// 		}
	// 	}
	// }

	// fmt.Println("\n")
	var ss Screens
	// Iterate through all of the crtcs and show some of their info.
	for _, crtc := range resources.Crtcs {

		info, err := randr.GetCrtcInfo(X, crtc, 0).Reply()
		if err != nil {
			return nil, err

		}
		if len(info.Outputs) == 0 {
			continue
		}

		// log.Printf("X: %d, Y: %d, Width: %d, Height: %d %v\n",
		// info.X, info.Y, info.Width, info.Height, info.Mode)
		// log.Println(spew.Sdump(info))
		ss = append(ss, Screen{
			X: info.X,
			Y: info.Y,
			W: info.Width,
			H: info.Height,
		})
	}

	// Tell RandR to send us events. (I think these are all of them, as of 1.3.)
	// err = randr.SelectInputChecked(X, root,
	// 	randr.NotifyMaskScreenChange|
	// 		randr.NotifyMaskCrtcChange|
	// 		randr.NotifyMaskOutputChange|
	// 		randr.NotifyMaskOutputProperty).Check()
	// if err != nil {
	// 	log.Fatal(err)
	// }

	// // Listen to events and just dump them to standard out.
	// // A more involved approach will have to read the 'U' field of
	// // RandrNotifyEvent, which is a union (really a struct) of type
	// // RanrNotifyDataUnion.
	// for {
	// 	ev, err := X.WaitForEvent()
	// 	if err != nil {
	// 		log.Fatal(err)
	// 	}
	// 	fmt.Println(ev)
	// }
	return ss, nil
}

func IsDarkmode() (bool, error) {
	home, err := homedir()
	if err != nil {
		return false, err
	}
	_, err = os.Stat(filepath.Join(home, ".config/darkmode"))
	if os.IsNotExist(err) {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	return true, nil
}

func panels() error {
	dm, err := IsDarkmode()
	if err != nil {
		return err
	}
	// log.Println(dm)
	screens, err := getScreens()
	if err != nil {
		return err
	}
	// spew.Dump(screens)

	sol := solarized
	if !dm {
		sol = sol.Inverse()
	}
	var xmobarConfigs []string
	defer func() {
		for _, v := range xmobarConfigs {
			os.Remove(v)
		}
	}()
	const height = 32
	for i, s := range screens {
		_ = s
		if i == 0 {

			trayerWidth := height * 8
			xmobarWidth := int(s.W) - int(trayerWidth)
			// topWidth := int(0.8 * float64(s.W))
			{
				f, err := runXmobar(xmobarTopTemplate, RenderContext{
					Sol:    sol,
					Width:  xmobarWidth,
					Screen: i,
					Xpos:   int(s.X),
					Ypos:   int(s.Y),
					Height: height,
				})
				if err != nil {
					return err
				}
				xmobarConfigs = append(xmobarConfigs, f)
			}

			if err := runTrayer(RenderContext{
				Sol:    sol,
				Width:  trayerWidth,
				Screen: i,
				Height: height,
			}); err != nil {
				return err
			}

			{
				f, err := runXmobar(xmobarBottomTemplate, RenderContext{
					Sol:    sol,
					Screen: i,
					Height: height,
				})
				if err != nil {
					return err
				}
				xmobarConfigs = append(xmobarConfigs, f)
			}

			continue
		}
		{
			f, err := runXmobar(xmobarTopTemplate, RenderContext{
				Sol:    sol,
				Screen: i,
				Height: height,
			})
			if err != nil {
				return err
			}
			xmobarConfigs = append(xmobarConfigs, f)
		}
		{
			f, err := runXmobar(xmobarBottomTemplate, RenderContext{
				Sol:    sol,
				Screen: i,
				Height: height,
			})
			if err != nil {
				return err
			}
			xmobarConfigs = append(xmobarConfigs, f)
		}
	}

	time.Sleep(250 * time.Millisecond)

	return nil
}

func runTrayer(rc RenderContext) error {
	cmd := exec.Command("trayer",
		"--edge", "top",
		"--align", "right",
		"--widthtype", "pixel",
		"--width", fmt.Sprint(rc.Width),
		"--height", fmt.Sprint(rc.Height),
		"--tint", "0x"+strings.TrimPrefix(rc.Sol.Base03.Color().Hex(), "#"),
		"--alpha", "0",
		"--transparent", "true",
		"--monitor", "primary",
	)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Start()
	return nil

}

func runXmobar(templ string, rc RenderContext) (string, error) {
	tpl := template.Must(template.New("foo").Parse(templ))
	tmpfile, err := ioutil.TempFile("", "xmobar")
	if err != nil {
		return "", err
	}
	defer tmpfile.Close()
	if err := tpl.Execute(tmpfile, rc); err != nil {
		return "", err
	}
	defer tmpfile.Close()
	cmd := exec.Command("xmobar", tmpfile.Name(), "-x", fmt.Sprint(rc.Screen))
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.Start()
	return tmpfile.Name(), nil
}

func homedir() (string, error) {
	u, err := user.Current()
	if err != nil {
		return "", err
	}
	return u.HomeDir, nil
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)

	if err := KillAll(context.Background(), "xmobar", "trayer"); err != nil {
		log.Fatal(err)
	}

	err := panels()
	if err != nil {
		log.Println(err)
		os.Exit(1)
	}

}

// Theme .
type RenderContext struct {
	Sol    cl.Solarized
	Width  int
	Xpos   int
	Ypos   int
	Screen int
	Height int
}

const (
	xmobarTopTemplate = `Config { font = "xft:Pragmata Pro:pixelsize=27"
       , additionalFonts = ["xft:Pragmata Pro:pixelsize=17", "xft:Pragmata Pro:pixelsize=16 :weight=bold"]
       , borderColor = "{{ .Sol.Base03 }}"
       , bgColor = "{{ .Sol.Base03 }}"
       , fgColor = "{{ .Sol.Base00 }}"
       , allDesktops = True
       , alpha = 255
       , border = NoBorder
       , hideOnStart = False
       , iconOffset = -1
       , iconRoot = "."
       , lowerOnStart = True
       , overrideRedirect = True
       , persistent = True
       , pickBroadest = False
     {{ if eq .Width 0 }}
       , position = TopSize C 100 {{ .Height }}
     {{ else }}
       , position =  Static { xpos = {{ .Xpos }}, ypos = {{ .Ypos }}, width = {{ .Width }}, height = {{ .Height }}  }
     {{ end }}
       , textOffset = -1
       , commands = [ Run NamedXPropertyLog "_XMONAD_LOG_TOP" "xm"
                    , Run DynNetwork ["-t", "<dev>:t<tx>:r<rx>k", "-L","0","-H","1000","--normal","{{ .Sol.Base00 }}","--high","{{ .Sol.Base1 }}"] 30
                    , Run MultiCpu ["-t", "<total>%", "-L","3","-H","80","--normal","{{ .Sol.Base1 }}","--high","{{ .Sol.Red }}", "-c", "0", "-p", "2"] 10
                    , Run Memory ["-t","<fc={{ .Sol.Base1 }}><available></fc>M"] 30
                    , Run DateZone "%a %b %-d %H:%M" "en_US.utf8" "Europe/Stockholm" "sthmlDate" 10
                    , Run DateZone "%H:%M" "en_US.utf8" "UTC" "time2" 10
                    , Run Battery ["-t", "<left>%", "-L", "10", "-H", "80"
                                  , "--"
                                  , "-L", "-15", "-H", "-5", "-l", "{{ .Sol.Base1 }}", "-m", "{{ .Sol.Base1 }}", "-h", "{{ .Sol.Base1 }}"] 600
                     -- , Run DiskIO [("/", "/ w<fc={{ .Sol.Base1 }}><write></fc> r<fc={{ .Sol.Base1 }}><read></fc>")] [] 20
       ]
       , sepChar = "%"
       , alignSep = "}{",
       , template = "%xm% }{ <fn=1>%dynnetwork%  <action=` + "`" + `x.info` + "`" + ` button=1>ma%memory%  cu%multicpu%  b%battery%</action></fn>  <fn=1><fc={{ .Sol.Green }}><action=` + "`" + `x.todo` + "`" + ` button=1>%sthmlDate%</action></fc> <fc={{ .Sol.Base00 }}>(%time2%)</fc>  </fn>"
`
	xmobarBottomTemplate = `Config { font = "xft:PT Sans Narrow-14:style=bold"
       , additionalFonts = ["xft:PT Sans Narrow-14", "xft:Pragmata Pro:pixelsize=18:weight:bold"]
       , borderColor = "{{ .Sol.Base02 }}"
       , bgColor = "{{ .Sol.Base03 }}"
       , fgColor = "{{ .Sol.Base00 }}"
       , allDesktops = True
       , alpha = 255
       , border = TopB
       , hideOnStart = False
       , iconOffset = -1
       , iconRoot = "."
       , lowerOnStart = True
       , overrideRedirect = True
       , persistent = True
       , pickBroadest = False
       , position = BottomSize C 100 16
       , textOffset = -1
       , commands = [ Run MPD ["-t",
                       " <statei> <artist> - <title> ",
                       "--", "-P", ">>", "-Z", "||", "-S", "><"] 10
                    -- , Run Com "fortune" ["~/.config-base/fortunes"] "quote" 9000
                    , Run NamedXPropertyLog "_XMONAD_LOG_BOTTOM" "xm"
                    , Run Kbd [("us(dvorak)", "DV"), ("us", "US")]
                    ]

       , sepChar = "%"
       , alignSep = "}{"
       , template = "  <fn=1><fc={{ .Sol.Base1 }}>%kbd%</fc></fn>   <fc={{ .Sol.Yellow }}>%xm%</fc> }{<fn=1><fc={{ .Sol.Green }}>%mpd%</fc></fn>"
`
)

func KillAll(ctx context.Context, names ...string) error {
	pss, err := process.Processes()
	if err != nil {
		return err
	}
	var wg sync.WaitGroup
loop:
	for _, p := range pss {
		name, err := p.Name()
		if err != nil {
			continue loop
		}
		for _, n := range names {
			if n == name {
				wg.Add(1)
				go func(p *process.Process) {
					defer wg.Done()
					p.KillWithContext(ctx)
				}(p)
			}
		}
		wg.Wait()
	}
	return nil
}
