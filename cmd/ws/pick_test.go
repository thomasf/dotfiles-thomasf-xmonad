//go:build linux

package main

import (
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

func TestPickGroups(t *testing.T) {
	workspaces := []string{"work", "pi.2", "NSP", "hello@test.bot", "hello.10", "hello.bot", "hello", "hello.2", "pi", "hello@a"}
	worktrees := []string{"hello@test", "hello@new", "pi@x"}
	got := pickGroups("pi", workspaces, worktrees)
	want := []pickGroup{
		{"pi", []pickEntry{{"pi.2", true}, {"pi@x", false}}},
		{"hello", []pickEntry{
			{"hello", true}, {"hello.2", true}, {"hello.10", true}, {"hello.bot", true},
			{"hello@a", true}, {"hello@new", false}, {"hello@test.bot", true},
		}},
		{"work", []pickEntry{{"work", true}}},
	}
	if !slices.EqualFunc(got, want, func(a, b pickGroup) bool {
		return a.project == b.project && slices.Equal(a.entries, b.entries)
	}) {
		t.Errorf("pickGroups =\n%v\nwant\n%v", got, want)
	}
}

func TestPickRows(t *testing.T) {
	rows, names := pickRows([]pickGroup{{"a&b", []pickEntry{{"a&b", true}, {"a&b@x", false}}}}, "#009c8f")
	wantRows := []string{
		"<b>a&amp;b</b>",
		"  a&amp;b",
		`  a&amp;b@x  <span foreground="#009c8f">worktree</span>`,
	}
	if !slices.Equal(rows, wantRows) || !slices.Equal(names, []string{"a&b", "a&b", "a&b@x"}) {
		t.Errorf("pickRows = %q, %q", rows, names)
	}
}

// fakeCommand puts a shell script named name, with body script, first
// in $PATH.
func fakeCommand(t *testing.T, name, script string) {
	t.Helper()
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, name), []byte("#!/bin/sh\n"+script+"\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	t.Setenv("PATH", dir+":"+os.Getenv("PATH"))
}

func TestRofiPickWorkspace(t *testing.T) {
	groups := []pickGroup{{"hello", []pickEntry{{"hello", true}, {"hello.bot", true}}}}
	for _, tt := range []struct {
		script, want string
		err          error
	}{
		{"echo '2 hb'", "hello.bot", nil},
		{"echo '-1 new ws '", "new ws", nil},
		{"echo '0 '", "hello", nil},
		{"echo '3 '", "", errFalse},
		{"exit 1", "", errFalse},
		{"echo '1 he'; exit 10", "", toggleWorktrees{"he"}},
	} {
		fakeCommand(t, "rofi", "cat >/dev/null; "+tt.script)
		got, err := rofiPickWorkspace("ws", 3, 20, "", false, groups)
		if got != tt.want || err != tt.err {
			t.Errorf("rofi %q: got %q, %v, want %q, %v", tt.script, got, err, tt.want, tt.err)
		}
	}
}

func TestWorktreePrefixes(t *testing.T) {
	t.Setenv("HOME", t.TempDir())
	hello := newRepo(t, "hello")
	dpg := newRepo(t, "dpg")
	if err := storeDir("dp", filepath.Join(dpg, "sub")); err != nil {
		t.Fatal(err)
	}
	dirs := []string{hello, hello + "@test", dpg, dpg + "@test", "/elsewhere/hello@x"}
	fakeCommand(t, "ghq", "printf '%s\\n' "+strings.Join(dirs, " "))
	got := worktreePrefixes([]string{"hello", "dp", "unknown"})
	if want := []string{"hello@test", "dp@test"}; !slices.Equal(got, want) {
		t.Errorf("worktreePrefixes = %v, want %v", got, want)
	}
}

func TestScreenAt(t *testing.T) {
	mons := []monitor{
		{"HDMI-0", rect{0, 2160, 3840, 2160}},
		{"DP-0", rect{3840, 2160, 3840, 2160}},
		{"DP-2", rect{3840, 0, 3840, 2160}},
	}
	// DP-0 is split into two screens side by side, DP-2 into two stacked.
	origins := []point{{0, 2160}, {3842, 2162}, {5762, 2162}, {3840, 0}, {3840, 1080}, {3842, 2162}}
	for _, tt := range []struct {
		o    point
		want rect
		mon  string
	}{
		{point{0, 2160}, rect{0, 2160, 3840, 2160}, "HDMI-0"},
		{point{3842, 2162}, rect{3842, 2162, 1920, 2158}, "DP-0"},
		{point{5762, 2162}, rect{5762, 2162, 1918, 2158}, "DP-0"},
		{point{3840, 0}, rect{3840, 0, 3840, 1080}, "DP-2"},
		{point{3840, 1080}, rect{3840, 1080, 3840, 1080}, "DP-2"},
	} {
		got, m, err := screenAt(tt.o, origins, mons)
		if err != nil || got != tt.want || m.name != tt.mon {
			t.Errorf("screenAt(%v) = %v, %s, %v, want %v, %s", tt.o, got, m.name, err, tt.want, tt.mon)
		}
	}
	if _, _, err := screenAt(point{99999, 0}, origins, mons); err == nil {
		t.Error("screenAt outside every monitor succeeded")
	}
}

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
