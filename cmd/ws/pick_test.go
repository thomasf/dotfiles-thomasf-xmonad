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
	got := pickGroups("pi", workspaces, map[string]bool{"hello.2": true}, worktrees)
	want := []pickGroup{
		{"pi", []pickEntry{{"pi.2", true, false}, {"pi@x", false, false}}},
		{"hello", []pickEntry{
			{"hello", true, false}, {"hello.2", true, true}, {"hello.10", true, false}, {"hello.bot", true, false},
			{"hello@a", true, false}, {"hello@new", false, false}, {"hello@test.bot", true, false},
		}},
		{"work", []pickEntry{{"work", true, false}}},
	}
	if !slices.EqualFunc(got, want, func(a, b pickGroup) bool {
		return a.project == b.project && slices.Equal(a.entries, b.entries)
	}) {
		t.Errorf("pickGroups =\n%v\nwant\n%v", got, want)
	}
}

func TestPickRows(t *testing.T) {
	rows, names := pickRows([]pickGroup{{"a&b", []pickEntry{{"a&b", true, false}, {"a&b.2", true, true}, {"a&b@x", false, false}}}}, "#009c8f", "#0064e4")
	wantRows := []string{
		"<b>a&amp;b</b>",
		"  a&amp;b",
		`  <span foreground="#0064e4">a&amp;b.2</span>`,
		`  a&amp;b@x  <span foreground="#009c8f">worktree</span>`,
	}
	if !slices.Equal(rows, wantRows) || !slices.Equal(names, []string{"a&b", "a&b", "a&b.2", "a&b@x"}) {
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
	groups := []pickGroup{{"hello", []pickEntry{{"hello", true, false}, {"hello.bot", true, false}}}}
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
		{"echo '2 hb'; exit 11", "", completeRow{"hello.bot"}},
		{"echo '0 he'; exit 11", "", completeRow{"hello"}},
		{"echo '-1 xyz'; exit 11", "", completeRow{"xyz"}},
		{"echo '1 he'; exit 12", "", toggleEmpty{"he"}},
		{"echo '1 hello@'; exit 11", "", chooseBranch{"hello"}},
		{"echo '2 a.b@'; exit 11", "", completeRow{"hello.bot"}},
	} {
		fakeCommand(t, "rofi", "cat >/dev/null; "+tt.script)
		got, err := rofiPickWorkspace("ws", 3, 20, "", false, false, groups)
		if got != tt.want || err != tt.err {
			t.Errorf("rofi %q: got %q, %v, want %q, %v", tt.script, got, err, tt.want, tt.err)
		}
	}
}

func TestBranchPickRows(t *testing.T) {
	infos := []branchInfo{
		{branchSource{branch: "a&b"}, "2 days ago", "Thomas Frössman", "feat: x & y"},
		{branchSource{branch: "feat/x", remote: "origin/feat/x"}, "32 hours ago", "R", "fix"},
		{branchSource{branch: "main"}, "5 hours ago", "Thomas Frössman", "chore"},
	}
	got := branchPickRows(infos, map[string]string{"main": "hello", "a&b": "hello@a-b"}, "#009c8f")
	want := []string{
		`<span fgalpha="55%">2 days ago    Thomas Frössman</span>  <b>a&amp;b</b> <span foreground="#009c8f">hello@a-b</span>  <span fgalpha="55%">feat: x &amp; y</span>`,
		`<span fgalpha="55%">32 hours ago  R</span>                <b>feat/x</b> <span foreground="#009c8f">origin</span>  <span fgalpha="55%">fix</span>`,
		`<span fgalpha="55%">5 hours ago   Thomas Frössman</span>  <b>main</b> <span foreground="#009c8f">hello</span>     <span fgalpha="55%">chore</span>`,
	}
	if !slices.Equal(got, want) {
		t.Errorf("branchPickRows =\n%s\nwant\n%s", strings.Join(got, "\n"), strings.Join(want, "\n"))
	}
}

func TestPickBranchWorktree(t *testing.T) {
	t.Setenv("HOME", t.TempDir())
	t.Setenv("DISPLAY", "")
	for k, v := range map[string]string{
		"GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@example.com",
		"GIT_COMMITTER_NAME": "t", "GIT_COMMITTER_EMAIL": "t@example.com",
	} {
		t.Setenv(k, v)
	}
	repo := newRepo(t, "hello")
	head := gitT(t, repo, "branch", "--show-current")
	// feat/x changed after the others, and baz, only on origin, last.
	commit := func(branch, date string) {
		t.Setenv("GIT_COMMITTER_DATE", date)
		c := gitT(t, repo, "commit-tree", "HEAD^{tree}", "-p", "HEAD", "-m", branch)
		gitT(t, repo, "branch", branch, c)
	}
	commit("feat/x", "2030-01-01T00:00:00")
	commit("baz", "2030-01-02T00:00:00")
	gitT(t, repo, "remote", "add", "origin", repo)
	gitT(t, repo, "fetch", "-q", "origin")
	gitT(t, repo, "branch", "-D", "baz")
	if err := storeDir("hello", repo); err != nil {
		t.Fatal(err)
	}
	fakeCommand(t, "ghq", "")
	fakeCommand(t, "notify-send", "")
	infos, err := branches(repo, "-committerdate")
	if err != nil || len(infos) != 4 || infos[0].remote != "origin/baz" || infos[1].branch != "feat/x" || infos[1].subject != "feat/x" {
		t.Fatalf("branches = %v, %v, want baz on origin, feat/x, then %s and test", infos, err, head)
	}

	// choose has rofi choose the row of branch.
	choose := func(branch string) string {
		return `n=$(grep -n -m1 -F '<b>` + branch + `</b>' | cut -d: -f1); echo $((n-1))`
	}
	for _, tt := range []struct {
		project, script, want string
		err                   error
	}{
		{"hello", choose(head), "hello", nil},
		{"hello", choose("test"), "hello@test", nil},
		{"hello", choose("feat/x"), "hello@feat-x", nil},
		{"hello", choose("feat/x"), "hello@feat-x", nil}, // checked out now
		{"hello", choose("baz"), "hello@baz", nil},
		{"hello", "cat >/dev/null; exit 1", "", errCancelled},
		{"unknown", "echo 0", "", errCancelled},
	} {
		fakeCommand(t, "rofi", tt.script)
		got, err := pickBranchWorktree(tt.project, 3, 20)
		if got != tt.want || err != tt.err {
			t.Errorf("%s, rofi %q: got %q, %v, want %q, %v", tt.project, tt.script, got, err, tt.want, tt.err)
		}
	}
	if got := gitT(t, repo+"@feat-x", "branch", "--show-current"); got != "feat/x" {
		t.Errorf("worktree feat-x is on %q, want feat/x", got)
	}
	if got := gitT(t, repo+"@baz", "rev-parse", "--abbrev-ref", "@{upstream}"); got != "origin/baz" {
		t.Errorf("worktree baz tracks %q, want origin/baz", got)
	}

	// Ctrl+r fetches branch fresh from remote up, then rofi shows it,
	// with what the user had typed.
	up := newRepo(t, "up")
	gitT(t, up, "branch", "fresh")
	gitT(t, repo, "remote", "add", "up", up)
	state := filepath.Join(t.TempDir(), "fetched")
	fakeCommand(t, "rofi", `if [ -e `+state+` ]; then echo "$@" >`+state+`; `+choose("fresh")+
		`; else cat >/dev/null; touch `+state+`; echo '0 fr'; exit 10; fi`)
	if got, err := pickBranchWorktree("hello", 3, 20); got != "hello@fresh" || err != nil {
		t.Errorf("after fetching, got %q, %v, want hello@fresh", got, err)
	}
	if args, _ := os.ReadFile(state); !strings.Contains(string(args), "-filter fr") {
		t.Errorf("rofi after fetching ran with %q, want -filter fr", args)
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
