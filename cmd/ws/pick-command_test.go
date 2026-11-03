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
	occupied := map[string]bool{"pi": true, "hello.2": true, "hello": true}
	got := pickGroups("pi", workspaces, map[string]bool{"hello.2": true}, occupied, worktrees)
	want := []PickGroup{
		{"pi", []PickEntry{{"pi", true, true, true, true}, {"pi.2", true, false, false, false}, {"pi@x", false, false, false, false}}},
		{"hello", []PickEntry{
			{"hello", true, false, false, true}, {"hello.2", true, true, false, true}, {"hello.10", true, false, false, false}, {"hello.bot", true, false, false, false},
			{"hello@a", true, false, false, false}, {"hello@new", false, false, false, false}, {"hello@test.bot", true, false, false, false},
		}},
		{"work", []PickEntry{{"work", true, false, false, false}}},
	}
	if !slices.EqualFunc(got, want, func(a, b PickGroup) bool {
		return a.project == b.project && slices.Equal(a.entries, b.entries)
	}) {
		t.Errorf("pickGroups =\n%v\nwant\n%v", got, want)
	}
}

func TestPickRows(t *testing.T) {
	rows, names := pickRows([]PickGroup{{"a&b", []PickEntry{
		{"a&b", true, true, true, true},
		{"a&b.2", true, true, false, false},
		{"a&b.3", true, false, false, false},
		{"a&b@x", false, false, false, false},
	}}}, "#009c8f", "#0064e4")
	wantRows := []string{
		"<b>a&amp;b</b>",
		`  <span foreground="#0064e4">a&amp;b</span>  <span fgalpha="60%">current</span>`,
		`  <span foreground="#0064e4" fgalpha="60%">a&amp;b.2</span>`,
		`  <span fgalpha="60%">a&amp;b.3</span>`,
		`  a&amp;b@x  <span foreground="#009c8f">worktree</span>`,
	}
	if !slices.Equal(rows, wantRows) || !slices.Equal(names, []string{"a&b", "a&b", "a&b.2", "a&b.3", "a&b@x"}) {
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
	groups := []PickGroup{{"hello", []PickEntry{{"hello", true, false, false, true}, {"hello.bot", true, false, false, true}}}}
	for _, tt := range []struct {
		script, want string
		err          error
	}{
		{"echo '2 hb'", "hello.bot", nil},
		{"echo '-1 new ws '", "new ws", nil},
		{"echo '0 '", "hello", nil},
		{"echo '3 '", "", errFalse},
		{"exit 1", "", errFalse},
		{"echo '1 he'; exit 10", "", ToggleWorktrees{"he"}},
		{"echo '2 hb'; exit 11", "", CompleteRow{"hello.bot"}},
		{"echo '0 he'; exit 11", "", CompleteRow{"hello"}},
		{"echo '-1 xyz'; exit 11", "", CompleteRow{"xyz"}},
		{"echo '1 he'; exit 12", "", ToggleEmpty{"he"}},
		{"echo '1 hello@'; exit 11", "", ChooseBranch{"hello"}},
		{"echo '2 a.b@'; exit 11", "", CompleteRow{"hello.bot"}},
	} {
		fakeCommand(t, "rofi", "cat >/dev/null; "+tt.script)
		got, err := rofiPickWorkspace("ws", 3, 20, "", false, false, groups)
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
