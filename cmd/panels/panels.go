//go:build linux

package main

import (
	"context"
	_ "embed"
	"encoding/json"
	"flag"
	"fmt"
	"io"

	"log/slog"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"text/template"
	"time"

	"github.com/jezek/xgb"
	"github.com/jezek/xgb/randr"
	"github.com/jezek/xgb/xproto"
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

// ScreenConfig specifies which panels/bars are displayed on a screen.
type ScreenConfig struct {
	Top         bool   `toml:"top"`
	TopSimple   bool   `toml:"top_simple"`
	TopTemplate string `toml:"top_template"`
	Bottom      bool   `toml:"bottom"`
	Trayer      bool   `toml:"trayer"`
}

func (sc ScreenConfig) hasTop() bool {
	return sc.Top || sc.TopSimple || sc.TopTemplate != ""
}

func (sc ScreenConfig) topTemplate() string {
	if sc.TopSimple || sc.TopTemplate == "simple" {
		return xmobarTopSimpleTemplate
	}
	return xmobarTopTemplate
}

// ComputerConfig defines panel configurations for a computer.
type ComputerConfig struct {
	Screens map[int]ScreenConfig `toml:"screens"`
	Default ScreenConfig         `toml:"default"`
}

// ConfigForScreen returns the ScreenConfig for the given screen index.
func (c ComputerConfig) ConfigForScreen(screenIndex int) ScreenConfig {
	if cfg, ok := c.Screens[screenIndex]; ok {
		return cfg
	}
	return c.Default
}

var (
	hostFlag  = flag.String("host", "", "override hostname for configuration")
	debugFlag = flag.Bool("log.debug", false, "log each step, and xmobar or trayer exiting while panels runs")

	computerConfigs = map[string]ComputerConfig{
		"flam": {
			Screens: map[int]ScreenConfig{
				0: {Top: true, Bottom: true, Trayer: true},
				// 1: {Top: true, TopSimple: true},
				// 2: {},
				// 3: {Top: true, TopSimple: true},
			},
			Default: ScreenConfig{},
		},
		"transwhale": {
			Screens: map[int]ScreenConfig{
				0: {Top: true, Bottom: true, Trayer: true},
			},
			Default: ScreenConfig{},
		},
	}

	defaultComputerConfig = ComputerConfig{
		Screens: map[int]ScreenConfig{
			0: {Top: true, Bottom: true, Trayer: true},
		},
		Default: ScreenConfig{Top: true, Bottom: true},
	}
)

func getComputerConfig(hostname string) ComputerConfig {
	hostname = strings.ToLower(strings.TrimSpace(strings.Split(hostname, ".")[0]))
	if cfg, ok := computerConfigs[hostname]; ok {
		slog.Debug("host config", "host", hostname, "config", cfg)
		return cfg
	}
	slog.Debug("no host config, using the default", "host", hostname, "config", defaultComputerConfig)
	return defaultComputerConfig
}

func getScreens() (Screens, error) {

	X, err := xgb.NewConn()
	if err != nil {
		return nil, fmt.Errorf("connect to the X server: %w", err)
	}
	defer X.Close()

	// Every extension must be initialized before it can be used.
	err = randr.Init(X)
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

	if slog.Default().Enabled(context.Background(), slog.LevelDebug) {
		for _, output := range resources.Outputs {
			info, err := randr.GetOutputInfo(X, output, 0).Reply()
			if err != nil {
				slog.Debug("cant get output info", "error", err)
				continue
			}

			if info.Connection == randr.ConnectionConnected {
				bestMode := info.Modes[0]
				for _, mode := range resources.Modes {
					if mode.Id == uint32(bestMode) {
						slog.Debug("size", "output", string(info.Name), "width",
							mode.Width, "height", mode.Height)
					}
				}
			}
		}
	}

	// fmt.Println("\n")
	var ss Screens
	// Iterate through all of the crtcs and show some of their info.
	for _, crtc := range resources.Crtcs {

		info, err := randr.GetCrtcInfo(X, crtc, 0).Reply()
		if err != nil {
			return nil, err

		}
		if len(info.Outputs) == 0 {
			slog.Debug("crtc without outputs, skipped", "crtc", crtc)
			continue
		}
		slog.Debug("screen", "index", len(ss), "crtc", crtc, "x", info.X, "y", info.Y, "width", info.Width, "height", info.Height)

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

// readPalette returns the colors of the theme in use, from the palette that
// dotfiles writes to ~/.config/theme/palette.json.
func readPalette() (map[string]string, error) {
	home, err := homedir()
	if err != nil {
		return nil, err
	}
	path := filepath.Join(home, ".config", "theme", "palette.json")
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	var p struct {
		Colors map[string]string `json:"colors"`
	}
	if err := json.Unmarshal(data, &p); err != nil {
		return nil, fmt.Errorf("%s: %w", path, err)
	}
	slog.Debug("palette", "path", path, "colors", len(p.Colors))
	return p.Colors, nil
}

func panels(hostname string) error {
	slog.Debug("process", "pid", os.Getpid(), "ppid", os.Getppid(), "pgid", syscall.Getpgrp())
	slog.Debug("environment", "DISPLAY", os.Getenv("DISPLAY"), "PATH", os.Getenv("PATH"))
	palette, err := readPalette()
	if err != nil {
		slog.Warn("palette unreadable, using grey", "err", err)
	}
	screens, err := getScreens()
	if err != nil {
		return err
	}
	// The running panels are only replaced once the screens are known, so
	// that they are kept without an X server to start new ones on.
	if err := KillAll(context.Background(), "xmobar", "trayer"); err != nil {
		return err
	}
	daemonLog = openDaemonLog()

	var xmobarConfigs []string
	defer func() {
		for _, v := range xmobarConfigs {
			os.Remove(v)
		}
	}()
	const height = 32

	compConfig := getComputerConfig(hostname)

	for i, s := range screens {
		sc := compConfig.ConfigForScreen(i)

		if sc.hasTop() {
			rc := RenderContext{
				Colors: palette,
				Screen: i,
				Height: height,
			}
			if sc.Trayer {
				trayerWidth := height * 8
				rc.Width = int(s.W) - int(trayerWidth)
				rc.Xpos = int(s.X)
				rc.Ypos = int(s.Y)
			}
			f, err := runXmobar(sc.topTemplate(), rc)
			if err != nil {
				return err
			}
			xmobarConfigs = append(xmobarConfigs, f)
		}

		if sc.Trayer {
			trayerWidth := height * 8
			if err := runTrayer(RenderContext{
				Colors: palette,
				Width:  trayerWidth,
				Screen: i,
				Height: height,
			}); err != nil {
				return err
			}
		}

		if sc.Bottom {
			f, err := runXmobar(xmobarBottomTemplate, RenderContext{
				Colors: palette,
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

	slog.Debug("removing xmobar configs and exiting", "configs", xmobarConfigs)
	return nil
}

func runTrayer(rc RenderContext) error {
	cmd := exec.Command("trayer",
		"--edge", "top",
		"--align", "right",
		"--widthtype", "pixel",
		"--width", fmt.Sprint(rc.Width),
		"--height", fmt.Sprint(rc.Height),
		"--tint", "0x"+strings.TrimPrefix(rc.C("bg_0"), "#"),
		"--alpha", "0",
		"--transparent", "true",
		"--monitor", "primary",
	)
	start(cmd)
	return nil
}

func runXmobar(templ string, rc RenderContext) (string, error) {
	tpl := template.Must(template.New("foo").Parse(templ))
	tmpfile, err := os.CreateTemp("", "xmobar")
	if err != nil {
		return "", err
	}
	defer tmpfile.Close()
	if err := tpl.Execute(tmpfile, rc); err != nil {
		return "", err
	}
	defer tmpfile.Close()
	slog.Debug("xmobar config", "path", tmpfile.Name(), "screen", rc.Screen,
		"x", rc.Xpos, "y", rc.Ypos, "width", rc.Width, "height", rc.Height)
	cmd := exec.Command("xmobar", tmpfile.Name(), "-x", fmt.Sprint(rc.Screen))
	start(cmd)
	return tmpfile.Name(), nil
}

// start starts cmd with its output going to daemonLog. A command that cannot
// be started is logged, so that the other panels still are. With -log.debug,
// an exit while panels still runs is logged as well.
func start(cmd *exec.Cmd) {
	cmd.Stdout = daemonLog
	cmd.Stderr = daemonLog
	if err := cmd.Start(); err != nil {
		slog.Error("start", "cmd", cmd.String(), "err", err)
		return
	}
	slog.Debug("started", "pid", cmd.Process.Pid, "cmd", cmd.String())
	if *debugFlag {
		go func() {
			err := cmd.Wait()
			slog.Warn("exited before panels did", "pid", cmd.Process.Pid, "cmd", cmd.String(), "err", err)
		}()
	}
}

// daemonLog is where trayer and xmobar write their output, or nil for
// /dev/null. They outlive panels, so they must not keep its output open: a
// caller that waits for it to close, such as a dotfiles script, would wait
// for them.
var daemonLog io.Writer

// openDaemonLog truncates and returns panels.log in the user's cache
// directory, or nil if it cannot be opened. The file is given to trayer
// and xmobar as it is, not through a pipe.
func openDaemonLog() io.Writer {
	dir, err := os.UserCacheDir()
	if err != nil {
		slog.Error("daemon log", "err", err)
		return nil
	}
	path := filepath.Join(dir, "panels.log")
	f, err := os.Create(path)
	if err != nil {
		slog.Error("daemon log", "err", err)
		return nil
	}
	slog.Debug("xmobar and trayer output", "path", path)
	return f
}

func homedir() (string, error) {
	u, err := user.Current()
	if err != nil {
		return "", err
	}
	return u.HomeDir, nil
}

func main() {
	flag.Parse()
	level := slog.LevelInfo
	if *debugFlag {
		level = slog.LevelDebug
	}
	slog.SetDefault(slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{Level: level})))

	hostname := *hostFlag
	if hostname == "" {
		hn, err := os.Hostname()
		if err != nil {
			slog.Error("hostname", "err", err)
		}
		hostname = hn
	}

	err := panels(hostname)
	if err != nil {
		slog.Error("panels", "err", err)
		os.Exit(1)
	}

}

// RenderContext is what the xmobar templates are rendered with.
type RenderContext struct {
	Colors map[string]string // the palette of the theme in use
	Width  int
	Xpos   int
	Ypos   int
	Screen int
	Height int
}

var (
	//go:embed xmobarTopTemplate
	xmobarTopTemplate string

	//go:embed xmobarTopSimpleTemplate
	xmobarTopSimpleTemplate string

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
				slog.Debug("killing", "name", name, "pid", p.Pid)
				wg.Add(1)
				go func(p *process.Process) {
					defer wg.Done()
					if err := p.KillWithContext(ctx); err != nil {
						slog.Error("kill", "name", name, "pid", p.Pid, "err", err)
					}
				}(p)
			}
		}
		wg.Wait()
	}
	return nil
}

// C returns the color name of the palette, or grey if it does not have it,
// as before dotfiles has written the palette. Templates use it as
// {{ .C "bg_0" }}.
func (rc RenderContext) C(name string) string {
	if c, ok := rc.Colors[name]; ok {
		return c
	}
	return "#808080"
}
