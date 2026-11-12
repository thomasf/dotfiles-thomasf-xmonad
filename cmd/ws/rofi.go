//go:build linux

package main

import (
	"bytes"
	"errors"
	"fmt"
	"image/png"
	"io"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
)

const rofiWidth = 85

func rofiCustomKey(err error) (int, bool) {
	if ee, ok := errors.AsType[*exec.ExitError](err); ok && ee.ExitCode() >= 10 {
		return ee.ExitCode() - 9, true
	}
	return 0, false
}

func parseRofiIndex(out []byte) (int, string, error) {
	indices, typed, err := parseRofiIndices(out)
	if err != nil || len(indices) == 0 {
		return 0, typed, err
	}
	return indices[0], typed, nil
}

func parseRofiIndices(out []byte) ([]int, string, error) {
	trimmed := strings.TrimRight(string(out), "\r\n")
	if trimmed == "" {
		return nil, "", nil
	}
	lines := strings.Split(trimmed, "\n")
	var indices []int
	typed := ""
	for _, l := range lines {
		l = strings.TrimRight(l, "\r")
		if l == "" {
			continue
		}
		idxStr, rest, hasRest := strings.Cut(l, " ")
		if hasRest && typed == "" {
			typed = rest
		}
		n, err := strconv.Atoi(idxStr)
		if err != nil {
			return nil, typed, err
		}
		indices = append(indices, n)
	}
	return indices, typed, nil
}

func rofiSizeArgs(args []string) (widthPct, heightPct int, rest []string, err error) {
	widthPct = rofiWidth
	for len(args) > 0 {
		var pct *int
		switch args[0] {
		case "-width":
			pct = &widthPct
		case "-height":
			pct = &heightPct
		default:
			return widthPct, heightPct, args, nil
		}
		if len(args) < 2 {
			return 0, 0, nil, fmt.Errorf("rofi: %s needs a percentage", args[0])
		}
		n, err := strconv.Atoi(args[1])
		if err != nil || n < 1 || n > 100 {
			return 0, 0, nil, fmt.Errorf("rofi: %s %q is not a percentage from 1 to 100", args[0], args[1])
		}
		*pct = n
		args = args[2:]
	}
	return widthPct, heightPct, args, nil
}

func rofiCommand(widthPct, heightPct int, theme string, args ...string) (*exec.Cmd, error) {
	path, err := rofiPath()
	if err != nil {
		return nil, err
	}
	return exec.Command(path, append(rofiPlacement(widthPct, heightPct, theme), args...)...), nil
}

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
	sc := s.center()
	return []string{"-m", m.name, "-theme-str", fmt.Sprintf(
		"window { location: north west; anchor: center; x-offset: %dpx; y-offset: %dpx; width: %dpx;%s } %s",
		sc.x-m.x, sc.y-m.y, s.w*widthPct/100, height, theme)}
}

// rofiCharWidth returns rofi's character width in pixels: measured with
// pango-view for its configured font and dpi and cached, else estimated;
// 0 if the configuration cannot be read.
func rofiCharWidth() float64 {
	dir, err := os.UserConfigDir()
	if err != nil {
		return 0
	}
	b, err := os.ReadFile(filepath.Join(dir, "rofi", "config.rasi"))
	if err != nil {
		return 0
	}
	font, dpi, ok := parseRofiFont(string(b))
	if !ok {
		return 0
	}
	key := fmt.Sprintf("%s\t%d", font, dpi)
	cache := xdgCacheFile("char_width")
	if w, ok := readCharWidth(cache, key); ok {
		return w
	}
	w, err := measureCharWidth(font, dpi)
	if err != nil {
		return estimateCharWidth(font, dpi)
	}
	writeAtomic(cache, fmt.Appendf(nil, "%s\t%g\n", key, w), 0o644)
	return w
}

var (
	rofiFont = regexp.MustCompile(`(?m)^\s*font:\s*"([^"]+)"`)
	rofiDPI  = regexp.MustCompile(`(?m)^\s*dpi:\s*([0-9]+)`)
)

// parseRofiFont returns the font and dpi set in a rofi configuration.
func parseRofiFont(config string) (font string, dpi int, ok bool) {
	fm, dm := rofiFont.FindStringSubmatch(config), rofiDPI.FindStringSubmatch(config)
	if fm == nil || dm == nil {
		return "", 0, false
	}
	dpi, err := strconv.Atoi(dm[1])
	if err != nil || dpi <= 0 {
		return "", 0, false
	}
	return fm[1], dpi, true
}

// readCharWidth returns the width cached in file for key.
func readCharWidth(file, key string) (float64, bool) {
	b, err := os.ReadFile(file)
	if err != nil {
		return 0, false
	}
	line := strings.TrimSpace(string(b))
	i := strings.LastIndex(line, "\t")
	if i < 0 || line[:i] != key {
		return 0, false
	}
	w, err := strconv.ParseFloat(line[i+1:], 64)
	return w, err == nil && w > 0
}

// charWidthSample is how many characters measureCharWidth renders.
const charWidthSample = 100

// measureCharWidth renders characters of font at dpi with pango-view and
// returns their width in pixels.
func measureCharWidth(font string, dpi int) (float64, error) {
	dir, err := os.MkdirTemp("", "ws-char-width")
	if err != nil {
		return 0, err
	}
	defer os.RemoveAll(dir)
	out := filepath.Join(dir, "sample.png")
	err = exec.Command("pango-view", "--font="+font, "--dpi="+strconv.Itoa(dpi), "--margin=0", "-q",
		"-o", out, "--text="+strings.Repeat("0", charWidthSample)).Run()
	if err != nil {
		return 0, err
	}
	f, err := os.Open(out)
	if err != nil {
		return 0, err
	}
	defer f.Close()
	c, err := png.DecodeConfig(f)
	if err != nil {
		return 0, err
	}
	return float64(c.Width) / charWidthSample, nil
}

const pointsPerInch = 72

// estimateCharWidth returns rofiCharEm of the font size at the end of
// font, in pixels at dpi, rounded to whole pixels as hinting does; 0
// if font has no size.
func estimateCharWidth(font string, dpi int) float64 {
	pt, err := strconv.ParseFloat(font[strings.LastIndex(font, " ")+1:], 64)
	if err != nil {
		return 0
	}
	return math.Round(pt * float64(dpi) / pointsPerInch * rofiCharEm)
}

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
