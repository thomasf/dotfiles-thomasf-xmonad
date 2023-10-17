package main

import (
	"context"
	_ "embed"
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
	"github.com/go-pa/colorlab"
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

	hn, err := os.Hostname()
	if err != nil {
		fmt.Println("hostname", err)
	} else if hn == "transwhale" {
		return ss[:1], nil
	}

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

	sol := colorlab.SelenizedDarkPalette.Solarized
	if !dm {
		sol = colorlab.SelenizedLightPalette.Solarized
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
	Sol    colorlab.Solarized
	Width  int
	Xpos   int
	Ypos   int
	Screen int
	Height int
}

var (
	//go:embed xmobarTopTemplate
	xmobarTopTemplate string

	//go:embed xmobarBottomTemplate
	xmobarBottomTemplate string
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
