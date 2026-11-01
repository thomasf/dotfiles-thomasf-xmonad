//go:build linux

package main

import (
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

func TestBranchPickRows(t *testing.T) {
	infos := []BranchInfo{
		{BranchSource{branch: "a&b"}, "2 days ago", "Thomas Frössman", "feat: x & y"},
		{BranchSource{branch: "feat/x", remote: "origin/feat/x"}, "32 hours ago", "R", "fix"},
		{BranchSource{branch: "main"}, "5 hours ago", "Thomas Frössman", "chore"},
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
	dir := newRepo(t, "hello")
	r := Repository(dir)
	head := gitT(t, dir, "branch", "--show-current")
	// feat/x changed after the others, and baz, only on origin, last.
	commit := func(branch, date string) {
		t.Setenv("GIT_COMMITTER_DATE", date)
		c := gitT(t, dir, "commit-tree", "HEAD^{tree}", "-p", "HEAD", "-m", branch)
		gitT(t, dir, "branch", branch, c)
	}
	commit("feat/x", "2030-01-01T00:00:00")
	commit("baz", "2030-01-02T00:00:00")
	gitT(t, dir, "remote", "add", "origin", dir)
	gitT(t, dir, "fetch", "-q", "origin")
	gitT(t, dir, "branch", "-D", "baz")
	if err := storeDir("hello", dir); err != nil {
		t.Fatal(err)
	}
	fakeCommand(t, "ghq", "")
	fakeCommand(t, "notify-send", "")
	infos, err := r.branches("-committerdate")
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
	if got := gitT(t, dir+"@feat-x", "branch", "--show-current"); got != "feat/x" {
		t.Errorf("worktree feat-x is on %q, want feat/x", got)
	}
	if got := gitT(t, dir+"@baz", "rev-parse", "--abbrev-ref", "@{upstream}"); got != "origin/baz" {
		t.Errorf("worktree baz tracks %q, want origin/baz", got)
	}

	// Ctrl+r fetches branch fresh from remote up, then rofi shows it,
	// with what the user had typed.
	up := newRepo(t, "up")
	gitT(t, up, "branch", "fresh")
	gitT(t, dir, "remote", "add", "up", up)
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
