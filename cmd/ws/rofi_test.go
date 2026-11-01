//go:build linux

package main

import (
	"os"
	"path/filepath"
	"slices"
	"testing"
)

func TestRofiPath(t *testing.T) {
	wrapper, real := t.TempDir(), t.TempDir()
	if err := os.WriteFile(filepath.Join(wrapper, "rofi"), []byte("#!/bin/sh\nexec ws rofi \"$@\"\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(real, "rofi"), []byte("#!/bin/sh\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	t.Setenv("PATH", wrapper+":"+t.TempDir()+":"+real)
	if got, err := rofiPath(); got != filepath.Join(real, "rofi") || err != nil {
		t.Errorf("rofiPath() = %q, %v, want %q", got, err, filepath.Join(real, "rofi"))
	}
	t.Setenv("PATH", wrapper)
	if _, err := rofiPath(); err == nil {
		t.Error("rofiPath() found only the wrapper and succeeded")
	}
}

func TestRofiSizeArgs(t *testing.T) {
	for _, tt := range []struct {
		args          []string
		width, height int
		rest          []string
		wantErr       bool
	}{
		{nil, rofiWidth, 0, nil, false},
		{[]string{"-dmenu", "-p", "x"}, rofiWidth, 0, []string{"-dmenu", "-p", "x"}, false},
		{[]string{"-height", "90", "-dmenu"}, rofiWidth, 90, []string{"-dmenu"}, false},
		{[]string{"-width", "50", "-dmenu"}, 50, 0, []string{"-dmenu"}, false},
		{[]string{"-width", "50", "-height", "90", "-dmenu"}, 50, 90, []string{"-dmenu"}, false},
		{[]string{"-height", "90", "-width", "100"}, 100, 90, []string{}, false},
		{[]string{"-dmenu", "-height", "90"}, rofiWidth, 0, []string{"-dmenu", "-height", "90"}, false},
		{[]string{"-height"}, 0, 0, nil, true},
		{[]string{"-width", "0"}, 0, 0, nil, true},
		{[]string{"-height", "101"}, 0, 0, nil, true},
		{[]string{"-width", "90%"}, 0, 0, nil, true},
		{[]string{"-height", "90", "-width"}, 0, 0, nil, true},
	} {
		width, height, rest, err := rofiSizeArgs(tt.args)
		if width != tt.width || height != tt.height || !slices.Equal(rest, tt.rest) || (err != nil) != tt.wantErr {
			t.Errorf("rofiSizeArgs(%q) = %d, %d, %q, %v, want %d, %d, %q, error %v",
				tt.args, width, height, rest, err, tt.width, tt.height, tt.rest, tt.wantErr)
		}
	}
}
