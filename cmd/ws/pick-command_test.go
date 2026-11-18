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
	dirs := []string{"custom"}
	occupied := map[string]bool{"pi": true, "hello.2": true, "hello": true}
	got := pickGroups("pi", workspaces, map[string]bool{"hello.2": true}, occupied, worktrees, dirs)
	want := []PickGroup{
		{"pi", []PickEntry{{"pi", true, true, true, true, ""}, {"pi.2", true, false, false, false, ""}, {"pi@x", false, false, false, false, "worktree"}}},
		{"custom", []PickEntry{{"custom", false, false, false, false, "dir"}}},
		{"hello", []PickEntry{
			{"hello", true, false, false, true, ""}, {"hello.2", true, true, false, true, ""}, {"hello.10", true, false, false, false, ""}, {"hello.bot", true, false, false, false, ""},
			{"hello@a", true, false, false, false, ""}, {"hello@new", false, false, false, false, "worktree"}, {"hello@test.bot", true, false, false, false, ""},
		}},
		{"work", []PickEntry{{"work", true, false, false, false, ""}}},
	}
	if !slices.EqualFunc(got, want, func(a, b PickGroup) bool {
		return a.project == b.project && slices.Equal(a.entries, b.entries)
	}) {
		t.Errorf("pickGroups =\n%v\nwant\n%v", got, want)
	}
}

func TestPickRows(t *testing.T) {
	rows, names := pickRows([]PickGroup{{"a&b", []PickEntry{
		{"a&b", true, true, true, true, ""},
		{"a&b.2", true, true, false, false, ""},
		{"a&b.3", true, false, false, false, ""},
		{"a&b@x", false, false, false, false, "worktree"},
		{"a&b@d", false, false, false, false, "dir"},
	}}}, "#009c8f", "#0064e4")
	wantRows := []string{
		"<b>a&amp;b</b>",
		`  <span foreground="#0064e4">a&amp;b</span>  <span fgalpha="60%">current</span>`,
		`  <span foreground="#0064e4" fgalpha="60%">a&amp;b.2</span>`,
		`  <span fgalpha="60%">a&amp;b.3</span>`,
		`  a&amp;b@x  <span foreground="#009c8f">worktree</span>`,
		`  a&amp;b@d  <span foreground="#009c8f">dir</span>`,
	}
	if !slices.Equal(rows, wantRows) || !slices.Equal(names, []string{"a&b", "a&b", "a&b.2", "a&b.3", "a&b@x", "a&b@d"}) {
		t.Errorf("pickRows = %q, %q", rows, names)
	}
}

func TestPickColumnChars(t *testing.T) {
	rows := []string{
		"<b>a&amp;b</b>",
		`  <span foreground="#0064e4">a&amp;b</span>  <span fgalpha="60%">current</span>`, // 14 characters
	}
	if got, want := pickColumnChars(rows), pickBallotChars+14+pickGapChars; got != want {
		t.Errorf("pickColumnChars = %d, want %d", got, want)
	}
}

func TestPickColumns(t *testing.T) {
	for _, c := range []struct{ col, avail, want int }{
		{20, pickMaxColumns*20 + pickChromeChars, pickMaxColumns},
		{20, 2*20 + pickChromeChars, 2},
		{20, 1000, pickMaxColumns},
		{50, 10, 1},
	} {
		if got := pickColumns(c.col, c.avail); got != c.want {
			t.Errorf("pickColumns(%d, %d) = %d, want %d", c.col, c.avail, got, c.want)
		}
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
	groups := []PickGroup{{"hello", []PickEntry{{"hello", true, false, false, true, ""}, {"hello.bot", true, false, false, true, ""}}}}
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
		{"echo '2 hello.bot'; exit 13", "", FillScreens{"hello.bot"}},
		{"echo '-1 typed'; exit 13", "", FillScreens{"typed"}},
		{"echo '1 he'; exit 14", "", ToggleDirs{"he"}},
	} {
		fakeCommand(t, "rofi", "cat >/dev/null; "+tt.script)
		got, err := rofiPickWorkspace("ws", 20, "", false, false, false, groups)
		if got != tt.want || err != tt.err {
			t.Errorf("rofi %q: got %q, %v, want %q, %v", tt.script, got, err, tt.want, tt.err)
		}
	}
}

func TestRofiPickWorkspacesMulti(t *testing.T) {
	groups := []PickGroup{{"hello", []PickEntry{{"hello", true, false, false, true, ""}, {"hello.bot", true, false, false, true, ""}}}}
	fakeCommand(t, "rofi", "cat >/dev/null; printf '0 \\n2 \\n'")
	got, err := rofiPickWorkspaces("ws", 20, "", false, false, false, groups)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	want := []string{"hello", "hello.bot"}
	if !slices.Equal(got, want) {
		t.Errorf("got %v, want %v", got, want)
	}
}

func TestRofiPickWorkspacesFlags(t *testing.T) {
	groups := []PickGroup{{"hello", []PickEntry{{"hello", true, false, false, true, ""}}}}
	flagsFile := filepath.Join(t.TempDir(), "flags")
	t.Setenv("FLAGS_FILE", flagsFile)
	fakeCommand(t, "rofi", `cat >/dev/null; printf "%s\n" "$@" > "$FLAGS_FILE"; echo '0 '`)
	_, err := rofiPickWorkspaces("ws", 20, "", false, false, false, groups)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	data, err := os.ReadFile(flagsFile)
	if err != nil {
		t.Fatalf("read flags: %v", err)
	}
	args := strings.Split(strings.TrimSpace(string(data)), "\n")
	checkFlag := func(flag, want string) {
		t.Helper()
		idx := slices.Index(args, flag)
		if idx == -1 || idx+1 >= len(args) || args[idx+1] != want {
			t.Errorf("flag %s: got %q, want %q", flag, args[idx+1:], want)
		}
	}
	checkFlag("-kb-accept-entry", "Control+m,Return,KP_Enter")
	checkFlag("-kb-accept-custom", "Control+Return,Control+j")
	checkFlag("-kb-custom-5", "Control+d")
	checkFlag("-kb-remove-char-forward", "Delete")
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
