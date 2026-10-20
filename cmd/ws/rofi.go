//go:build linux

package main

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"syscall"
)

// rofiWidth is how wide rofi is, in percent of the xmonad screen, unless
// the caller says otherwise. It is the width in config.rasi, which rofi
// takes as a percentage of the monitor instead.
const rofiWidth = 75

// runRofi runs rofi with args, placed on the current xmonad screen. It
// replaces ws with rofi.
func runRofi(args []string) error {
	path, err := rofiPath()
	if err != nil {
		return err
	}
	argv := append([]string{"rofi"}, rofiPlacement(rofiWidth, 0, "")...)
	return syscall.Exec(path, append(argv, args...), os.Environ())
}

// rofiCommand returns a command running rofi with args, its window
// centred on the xmonad screen that shows the current workspace and
// widthPct percent as wide as that screen and, unless heightPct is 0,
// heightPct percent as high; else rofi makes it as high as its lines
// need. theme, if not empty, is further rofi theme for the window.
func rofiCommand(widthPct, heightPct int, theme string, args ...string) (*exec.Cmd, error) {
	path, err := rofiPath()
	if err != nil {
		return nil, err
	}
	return exec.Command(path, append(rofiPlacement(widthPct, heightPct, theme), args...)...), nil
}

// rofiPlacement returns the rofi arguments for rofiCommand. rofi knows
// only monitors, not the screens xmonad splits them into, so it is told
// the monitor and the offset of the screen's centre within it. If the
// screen cannot be found, the width and height are percentages of the
// monitor, and rofi places the window itself.
func rofiPlacement(widthPct, heightPct int, theme string) []string {
	s, m, err := currentScreen()
	if err != nil {
		height := ""
		if heightPct > 0 {
			height = fmt.Sprintf(" height: %d%%;", heightPct)
		}
		return []string{"-theme-str", fmt.Sprintf("window { width: %d%%;%s } %s", widthPct, height, theme)}
	}
	height := ""
	if heightPct > 0 {
		height = fmt.Sprintf(" height: %dpx;", s.h*heightPct/100)
	}
	return []string{"-m", m.name, "-theme-str", fmt.Sprintf(
		"window { location: north west; anchor: center; x-offset: %dpx; y-offset: %dpx; width: %dpx;%s } %s",
		s.x-m.x+s.w/2, s.y-m.y+s.h/2, s.w*widthPct/100, height, theme)}
}

// rofiPath returns the path of the first rofi in $PATH that is not a
// wrapper script running ws rofi.
func rofiPath() (string, error) {
	for _, dir := range filepath.SplitList(os.Getenv("PATH")) {
		p := filepath.Join(dir, "rofi")
		fi, err := os.Stat(p)
		if err != nil || fi.IsDir() || fi.Mode()&0o111 == 0 || isRofiWrapper(p) {
			continue
		}
		return p, nil
	}
	return "", errors.New("no rofi in $PATH")
}

func isRofiWrapper(path string) bool {
	f, err := os.Open(path)
	if err != nil {
		return false
	}
	defer f.Close()
	b, _ := io.ReadAll(io.LimitReader(f, 1024))
	return bytes.HasPrefix(b, []byte("#!")) && bytes.Contains(b, []byte("ws rofi"))
}
