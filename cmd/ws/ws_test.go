//go:build linux

package main

import (
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"testing"
)

func TestWorkspace(t *testing.T) {
	for _, tt := range []struct {
		w                                 workspace
		prefix, project, worktree, suffix string
		hasSuffix                         bool
	}{
		{"hello", "hello", "hello", "", "", false},
		{"hello.1", "hello", "hello", "", "1", true},
		{"hello@test", "hello@test", "hello", "test", "", false},
		{"hello@test.bot", "hello@test", "hello", "test", "bot", true},
		{"tl.log.x", "tl", "tl", "", "log.x", true},
		{"a@b@c.d", "a@b@c", "a", "b@c", "d", true},
	} {
		suffix, hasSuffix := tt.w.suffix()
		if got := tt.w.prefix(); got != tt.prefix {
			t.Errorf("%q.prefix() = %q, want %q", tt.w, got, tt.prefix)
		}
		if got := tt.w.project(); got != tt.project {
			t.Errorf("%q.project() = %q, want %q", tt.w, got, tt.project)
		}
		if got := tt.w.worktree(); got != tt.worktree {
			t.Errorf("%q.worktree() = %q, want %q", tt.w, got, tt.worktree)
		}
		if suffix != tt.suffix || hasSuffix != tt.hasSuffix {
			t.Errorf("%q.suffix() = %q, %v, want %q, %v", tt.w, suffix, hasSuffix, tt.suffix, tt.hasSuffix)
		}
	}
}

// newRepo creates a repository named name in a new directory, with a
// subdirectory sub and a worktree <name>@test beside it.
func newRepo(t *testing.T, name string) string {
	t.Helper()
	root, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	repo := filepath.Join(root, name)
	if err := os.MkdirAll(filepath.Join(repo, "sub"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(repo, "sub", "f"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	for _, args := range [][]string{
		{"init", "-q"},
		{"add", "."},
		{"-c", "user.name=t", "-c", "user.email=t@example.com", "commit", "-qm", "init"},
		{"worktree", "add", "-q", "-b", "test", "../" + name + "@test"},
	} {
		cmd := exec.Command("git", args...)
		cmd.Dir = repo
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("git %v: %v\n%s", args, err, out)
		}
	}
	return repo
}

func TestWorktreeDir(t *testing.T) {
	repo := newRepo(t, "hello")
	wt := repo + "@test"
	for _, tt := range []struct{ dir, want string }{
		{repo, wt},
		{filepath.Join(repo, "sub"), filepath.Join(wt, "sub")},
		{wt, wt},
	} {
		got, err := worktreeDir(tt.dir, "test")
		if err != nil || got != tt.want {
			t.Errorf("worktreeDir(%q, test) = %q, %v, want %q", tt.dir, got, err, tt.want)
		}
	}
	if _, err := worktreeDir(repo, "nosuch"); !errors.Is(err, errNoDir) {
		t.Errorf("worktreeDir(%q, nosuch) error = %v, want errNoDir", repo, err)
	}
}

func TestWorkspaceDirStored(t *testing.T) {
	t.Setenv("HOME", t.TempDir())
	repo := newRepo(t, "dpg")
	sub := filepath.Join(repo, "sub")
	if err := storeDir("dp", sub); err != nil {
		t.Fatal(err)
	}
	if err := storeDir("dp@other", "/elsewhere"); err != nil {
		t.Fatal(err)
	}
	for _, tt := range []struct {
		w    workspace
		want string
	}{
		{"dp", sub},
		{"dp.1", sub},
		{"dp@test.bot", filepath.Join(repo+"@test", "sub")},
		{"dp@other.bot", "/elsewhere"},
	} {
		got, err := workspaceDir(tt.w, noPick)
		if err != nil || got != tt.want {
			t.Errorf("workspaceDir(%q) = %q, %v, want %q", tt.w, got, err, tt.want)
		}
	}

	if err := storeDir("dp", ""); err != nil {
		t.Fatal(err)
	}
	if got, err := storedDir("dp"); got != "" || err != nil {
		t.Errorf("storedDir(dp) after removal = %q, %v, want \"\", nil", got, err)
	}
	if err := storeDir("dp", ""); err != nil {
		t.Errorf("removing a missing stored directory: %v", err)
	}
	if _, err := storedDir("../x"); err == nil {
		t.Error("storedDir(../x) succeeded")
	}
}

func TestEnvFile(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	repo := newRepo(t, "dpg")
	if err := storeDir("dp", repo); err != nil {
		t.Fatal(err)
	}
	if err := storeDir("dp@other", "/elsewhere"); err != nil {
		t.Fatal(err)
	}
	wsenv := filepath.Join(home, ".config", "wsenv")
	for _, tt := range []struct {
		w    workspace
		want string
	}{
		{"dp.1", "dpg"},
		{"dp@test.bot", "dpg@test"},
		{"dp@other", "elsewhere"},
		{"dp@nosuch.bot", "dp@nosuch"},
	} {
		got, err := envFile(tt.w)
		if want := filepath.Join(wsenv, tt.want); err != nil || got != want {
			t.Errorf("envFile(%q) = %q, %v, want %q", tt.w, got, err, want)
		}
	}

	if _, err := readEnv(filepath.Join(wsenv, "dpg")); !errors.Is(err, errFalse) {
		t.Errorf("readEnv of a missing file: error = %v, want errFalse", err)
	}
}

func TestValidWorktreeName(t *testing.T) {
	for name, want := range map[string]bool{
		"test": true, "fix-auth": true, "v2": true,
		"": false, "a.b": false, "a/b": false, "a@b": false, "-x": false, "a b": false,
	} {
		if got := validWorktreeName(name); got != want {
			t.Errorf("validWorktreeName(%q) = %v, want %v", name, got, want)
		}
	}
}

func gitT(t *testing.T, dir string, args ...string) string {
	t.Helper()
	out, err := git(dir, args...)
	if err != nil {
		t.Fatal(err)
	}
	return out
}

func TestWorktrees(t *testing.T) {
	repo := newRepo(t, "dpg")
	gitT(t, repo, "worktree", "add", "-q", "-b", "other", "../unrelated")
	gitT(t, repo, "branch", "existing")
	head := gitT(t, repo, "branch", "--show-current")

	main, rel, err := mainCheckout(filepath.Join(repo+"@test", "sub"))
	if err != nil || main != repo || rel != "sub" {
		t.Fatalf("mainCheckout(worktree/sub) = %q, %q, %v, want %q, sub", main, rel, err, repo)
	}

	list := func() []worktree {
		t.Helper()
		wts, err := listWorktrees("dp", repo)
		if err != nil {
			t.Fatal(err)
		}
		return wts
	}
	want := []worktree{{"dp", repo, head}, {"dp@test", repo + "@test", "test"}}
	if got := list(); !slices.Equal(got, want) {
		t.Errorf("listWorktrees = %v, want %v", got, want)
	}

	if err := addWorktree(repo, "new", exactBranch(repo, "new")); err != nil {
		t.Fatal(err)
	}
	if err := addWorktree(repo, "x", exactBranch(repo, "existing")); err != nil {
		t.Fatal(err)
	}
	if err := addWorktree(repo, "new", exactBranch(repo, "new2")); err == nil {
		t.Error("adding worktree new twice succeeded")
	}
	// The main checkout comes first, then the worktrees by directory.
	want = []worktree{
		{"dp", repo, head},
		{"dp@new", repo + "@new", "new"},
		{"dp@test", repo + "@test", "test"},
		{"dp@x", repo + "@x", "existing"},
	}
	if got := list(); !slices.Equal(got, want) {
		t.Errorf("after adding, listWorktrees = %v, want %v", got, want)
	}

	if err := os.WriteFile(filepath.Join(repo+"@x", "dirty"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	if err := removeWorktree(repo, "x", false); err == nil {
		t.Error("removing a worktree with changes succeeded without force")
	}
	if err := removeWorktree(repo, "x", true); err != nil {
		t.Error(err)
	}
	if err := removeWorktree(repo, "new", false); err != nil {
		t.Error(err)
	}
	want = slices.Delete(want, 3, 4)
	want = slices.Delete(want, 1, 2)
	if got := list(); !slices.Equal(got, want) {
		t.Errorf("after removing, listWorktrees = %v, want %v", got, want)
	}
	if out := gitT(t, repo, "branch", "--list", "new"); out == "" {
		t.Error("removing worktree new deleted its branch")
	}
}

func TestMatchingBranches(t *testing.T) {
	repo := newRepo(t, "hello")
	for _, b := range []string{"existing", "team/bar", "feature/foo", "baz", "other/baz-x"} {
		gitT(t, repo, "branch", b)
	}
	// Make feature/foo and baz exist only on the remote origin.
	gitT(t, repo, "remote", "add", "origin", repo)
	gitT(t, repo, "fetch", "-q", "origin")
	gitT(t, repo, "branch", "-D", "feature/foo", "baz")

	for name, want := range map[string][]branchSource{
		"existing":    {{branch: "existing"}},
		"bar":         {{branch: "team/bar"}},
		"team-bar":    {{branch: "team/bar"}},
		"feature-foo": {{branch: "feature/foo", remote: "origin/feature/foo"}},
		"foo":         {{branch: "feature/foo", remote: "origin/feature/foo"}},
		"baz-x":       {{branch: "other/baz-x"}},
		"baz":         {{branch: "baz", remote: "origin/baz"}},
		"test":        {{branch: "test"}},
		"none":        nil,
	} {
		got, err := matchingBranches(repo, name)
		if err != nil || !slices.Equal(got, want) {
			t.Errorf("matchingBranches(%q) = %v, %v, want %v", name, got, err, want)
		}
	}

	if got, want := exactBranch(repo, "baz"), (branchSource{branch: "baz", remote: "origin/baz"}); got != want {
		t.Errorf("exactBranch(baz) = %v, want %v", got, want)
	}
	if got, want := exactBranch(repo, "foo"), (branchSource{branch: "foo", isNew: true}); got != want {
		t.Errorf("exactBranch(foo) = %v, want %v", got, want)
	}

	if err := addWorktree(repo, "feature-foo", branchSource{branch: "feature/foo", remote: "origin/feature/foo"}); err != nil {
		t.Fatal(err)
	}
	if got := gitT(t, repo+"@feature-foo", "rev-parse", "--abbrev-ref", "@{upstream}"); got != "origin/feature/foo" {
		t.Errorf("worktree feature-foo tracks %q, want origin/feature/foo", got)
	}
}

func TestDescribeHead(t *testing.T) {
	for log, want := range map[string]string{
		"HEAD -> main, origin/main abc1234": "main (abc1234)",
		"HEAD -> main abc1234":              "main (abc1234)",
		"abc1234":                           "abc1234",
		"tag: v1, origin/x abc1234":         "abc1234",
	} {
		if got := describeHead(log); got != want {
			t.Errorf("describeHead(%q) = %q, want %q", log, got, want)
		}
	}
}
