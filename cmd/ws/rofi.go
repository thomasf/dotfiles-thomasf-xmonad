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
