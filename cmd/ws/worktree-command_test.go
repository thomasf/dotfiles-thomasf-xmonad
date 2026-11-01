//go:build linux

package main

import (
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

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

func gitT(t *testing.T, dir string, args ...string) string {
	t.Helper()
	out, err := Repository(dir).git(args...)
	if err != nil {
		t.Fatal(err)
	}
	return out
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

func TestWorktreeArg(t *testing.T) {
	for _, tt := range []struct {
		arg, project, name, branch string
	}{
		{"repo@x", "repo", "x", "x"},
		{"repo@feat/x", "repo", "feat-x", "feat/x"},
		{"repo@a/b/c", "repo", "a-b-c", "a/b/c"},
	} {
		project, name, branch, err := worktreeArg(tt.arg)
		if err != nil || project != tt.project || name != tt.name || branch != tt.branch {
			t.Errorf("worktreeArg(%q) = %q, %q, %q, %v, want %q, %q, %q", tt.arg, project, name, branch, err, tt.project, tt.name, tt.branch)
		}
	}
	for _, arg := range []string{"repo@", "repo@a.b", "repo@a@b", "repo@-x"} {
		if _, _, _, err := worktreeArg(arg); err == nil {
			t.Errorf("worktreeArg(%q): no error", arg)
		}
	}
}

func TestWorktrees(t *testing.T) {
	repoDir := newRepo(t, "dpg")
	r := Repository(repoDir)
	gitT(t, repoDir, "worktree", "add", "-q", "-b", "other", "../unrelated")
	gitT(t, repoDir, "branch", "existing")
	head := gitT(t, repoDir, "branch", "--show-current")

	main, rel, err := mainCheckout(filepath.Join(repoDir+"@test", "sub"))
	if err != nil || main != r || rel != "sub" {
		t.Fatalf("mainCheckout(worktree/sub) = %q, %q, %v, want %q, sub", main, rel, err, repoDir)
	}

	list := func() []WorkTree {
		t.Helper()
		wts, err := r.listWorktrees("dp")
		if err != nil {
			t.Fatal(err)
		}
		return wts
	}
	want := []WorkTree{{"dp", repoDir, head}, {"dp@test", repoDir + "@test", "test"}}
	if got := list(); !slices.Equal(got, want) {
		t.Errorf("listWorktrees = %v, want %v", got, want)
	}

	if err := r.addWorktree("new", r.exactBranch("new")); err != nil {
		t.Fatal(err)
	}
	if err := r.addWorktree("x", r.exactBranch("existing")); err != nil {
		t.Fatal(err)
	}
	if err := r.addWorktree("new", r.exactBranch("new2")); err == nil {
		t.Error("adding worktree new twice succeeded")
	}
	// The main checkout comes first, then the worktrees by directory.
	want = []WorkTree{
		{"dp", repoDir, head},
		{"dp@new", repoDir + "@new", "new"},
		{"dp@test", repoDir + "@test", "test"},
		{"dp@x", repoDir + "@x", "existing"},
	}
	if got := list(); !slices.Equal(got, want) {
		t.Errorf("after adding, listWorktrees = %v, want %v", got, want)
	}

	if err := os.WriteFile(filepath.Join(repoDir+"@x", "dirty"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	if err := r.removeWorktree("x", false); err == nil {
		t.Error("removing a worktree with changes succeeded without force")
	}
	if err := r.removeWorktree("x", true); err != nil {
		t.Error(err)
	}
	if err := r.removeWorktree("new", false); err != nil {
		t.Error(err)
	}
	want = slices.Delete(want, 3, 4)
	want = slices.Delete(want, 1, 2)
	if got := list(); !slices.Equal(got, want) {
		t.Errorf("after removing, listWorktrees = %v, want %v", got, want)
	}
	if out := gitT(t, repoDir, "branch", "--list", "new"); out == "" {
		t.Error("removing worktree new deleted its branch")
	}
}

func TestMatchingBranches(t *testing.T) {
	dir := newRepo(t, "hello")
	r := Repository(dir)
	for _, b := range []string{"existing", "team/bar", "feature/foo", "baz", "other/baz-x"} {
		gitT(t, dir, "branch", b)
	}
	// Make feature/foo and baz exist only on the remote origin.
	gitT(t, dir, "remote", "add", "origin", dir)
	gitT(t, dir, "fetch", "-q", "origin")
	gitT(t, dir, "branch", "-D", "feature/foo", "baz")
	h := gitT(t, dir, "rev-parse", "--short", "HEAD")

	for name, want := range map[string][]BranchSource{
		"existing":    {{branch: "existing", hash: h}},
		"bar":         {{branch: "team/bar", hash: h}},
		"team-bar":    {{branch: "team/bar", hash: h}},
		"feature-foo": {{branch: "feature/foo", remote: "origin/feature/foo", hash: h}},
		"foo":         {{branch: "feature/foo", remote: "origin/feature/foo", hash: h}},
		"baz-x":       {{branch: "other/baz-x", hash: h}},
		"baz":         {{branch: "baz", remote: "origin/baz", hash: h}},
		"test":        {{branch: "test", hash: h}},
		"none":        nil,
	} {
		got, err := r.matchingBranches(name)
		if err != nil || !slices.Equal(got, want) {
			t.Errorf("matchingBranches(%q) = %v, %v, want %v", name, got, err, want)
		}
	}

	if got, want := r.exactBranch("baz"), (BranchSource{branch: "baz", remote: "origin/baz", hash: h}); got != want {
		t.Errorf("exactBranch(baz) = %v, want %v", got, want)
	}
	if got, want := r.exactBranch("foo"), (BranchSource{branch: "foo", isNew: true}); got != want {
		t.Errorf("exactBranch(foo) = %v, want %v", got, want)
	}

	if err := r.addWorktree("feature-foo", BranchSource{branch: "feature/foo", remote: "origin/feature/foo"}); err != nil {
		t.Fatal(err)
	}
	if got := gitT(t, dir+"@feature-foo", "rev-parse", "--abbrev-ref", "@{upstream}"); got != "origin/feature/foo" {
		t.Errorf("worktree feature-foo tracks %q, want origin/feature/foo", got)
	}
}

func TestNewBranches(t *testing.T) {
	dir := newRepo(t, "hello")
	r := Repository(dir)
	gitT(t, dir, "branch", "-M", "main")
	for _, remoteName := range []string{"origin", "upstream"} {
		gitT(t, dir, "remote", "add", remoteName, dir)
		gitT(t, dir, "fetch", "-q", remoteName)
		gitT(t, dir, "remote", "set-head", remoteName, "main")
	}
	hash := gitT(t, dir, "rev-parse", "--short", "HEAD")

	// The age of the remote-tracking branches depends on the clock.
	noAge := func(srcs []BranchSource) []BranchSource {
		for i := range srcs {
			srcs[i].note, _, _ = strings.Cut(srcs[i].note, ", updated ")
		}
		return srcs
	}
	fetched := []BranchSource{
		{branch: "x", isNew: true, fetch: "upstream", from: "upstream/main", note: "default branch on upstream"},
		{branch: "x", isNew: true, fetch: "origin", from: "origin/main", note: "default branch on origin"},
	}
	lastFetched := []BranchSource{
		{branch: "x", isNew: true, from: "upstream/main", hash: hash, note: "default branch on upstream"},
		{branch: "x", isNew: true, from: "origin/main", hash: hash, note: "default branch on origin"},
	}
	got, def, err := newBranches(r, "x")
	if def != 0 {
		t.Errorf("newBranches on main suggests %d, want 0", def)
	}
	want := slices.Concat(fetched,
		[]BranchSource{{branch: "x", isNew: true, from: "main", hash: hash, note: "default branch, checked out"}},
		lastFetched)
	if err != nil || !slices.Equal(noAge(got), want) {
		t.Errorf("newBranches on main =\n%v, %v, want\n%v", got, err, want)
	}

	// The remote that main tracks comes first.
	gitT(t, dir, "branch", "-q", "--set-upstream-to", "origin/main", "main")
	got, _, err = newBranches(r, "x")
	if err != nil || len(got) < 2 || got[0].fetch != "origin" || got[1].fetch != "upstream" {
		t.Errorf("newBranches with main tracking origin = %v, %v, want origin first", got, err)
	}
	gitT(t, dir, "branch", "-q", "--set-upstream-to", "upstream/main", "main")

	gitT(t, dir, "switch", "-q", "-c", "other")
	got, _, err = newBranches(r, "x")
	want = slices.Concat(fetched,
		[]BranchSource{{branch: "x", isNew: true, from: "main", hash: hash, note: "default branch"}},
		lastFetched,
		[]BranchSource{{branch: "x", isNew: true, from: "other", hash: hash, note: "checked out"}})
	if err != nil || !slices.Equal(noAge(got), want) {
		t.Errorf("newBranches on other =\n%v, %v, want\n%v", got, err, want)
	}

	if err := r.addWorktree("x", want[3]); err != nil {
		t.Fatal(err)
	}
	if got := gitT(t, dir+"@x", "branch", "--show-current"); got != "x" {
		t.Errorf("worktree x is on %q, want x", got)
	}
	if out, err := Repository(dir+"@x").git("rev-parse", "--abbrev-ref", "@{upstream}"); err == nil {
		t.Errorf("new branch x tracks %q, want no upstream", out)
	}
}

func TestNewBranchesAhead(t *testing.T) {
	dir := newRepo(t, "hello")
	r := Repository(dir)
	gitT(t, dir, "branch", "-M", "main")
	gitT(t, dir, "remote", "add", "origin", dir)
	commit := func(msg string, args ...string) {
		gitT(t, dir, append([]string{"-c", "user.name=t", "-c", "user.email=t@example.com", "commit", "-q", "--allow-empty", "-m", msg}, args...)...)
	}
	commit("pushed")
	gitT(t, dir, "fetch", "-q", "origin")
	gitT(t, dir, "remote", "set-head", "origin", "main")

	// The local default branch is the second way, after fetching origin.
	for _, tt := range []struct {
		do   func()
		note string
		def  int
	}{
		{func() {}, "default branch, checked out", 0},
		{func() { gitT(t, dir, "reset", "-q", "--hard", "HEAD~1") }, "default branch, checked out, 1 commit behind origin as last fetched", 0},
		{func() { commit("a"); commit("b") }, "default branch, checked out, 2 commits not on origin, origin has 1 not here", 1},
		{func() { gitT(t, dir, "reset", "-q", "--hard", "origin/main"); commit("unpushed") }, "default branch, checked out, 1 commit not on origin", 1},
		// Amending the pushed commit: the remote has the old version.
		{func() { gitT(t, dir, "reset", "-q", "--hard", "origin/main"); commit("pushed, amended", "--amend") }, "default branch, checked out, 1 commit not on origin, origin has 1 not here", 1},
	} {
		tt.do()
		srcs, def, err := newBranches(r, "x")
		if err != nil || len(srcs) < 2 || srcs[1].note != tt.note || def != tt.def {
			t.Errorf("newBranches = %v, %d, %v, want %q at 1 and suggesting %d", srcs, def, err, tt.note, tt.def)
		}
	}
}

func TestFetchSource(t *testing.T) {
	dir := newRepo(t, "hello")
	r := Repository(dir)
	pub := filepath.Join(t.TempDir(), "pub")
	gitT(t, dir, "clone", "-q", dir, pub)
	gitT(t, dir, "remote", "add", "pub", pub)
	gitT(t, dir, "fetch", "-q", "pub")
	gitT(t, dir, "remote", "set-head", "pub", "--auto")
	stale := gitT(t, dir, "symbolic-ref", "--short", "refs/remotes/pub/HEAD")

	// The remote moves on and renames its default branch.
	gitT(t, pub, "-c", "user.name=t", "-c", "user.email=t@example.com", "commit", "-q", "--allow-empty", "-m", "b")
	gitT(t, pub, "branch", "-M", "trunk")
	head := gitT(t, pub, "rev-parse", "HEAD")

	src, err := fetchSource(r, "", BranchSource{branch: "y", isNew: true, fetch: "pub", from: stale}, false)
	if err != nil {
		t.Fatal(err)
	}
	if want := (BranchSource{branch: "y", isNew: true, from: "pub/trunk", hash: gitT(t, pub, "rev-parse", "--short", "HEAD")}); src != want {
		t.Errorf("fetchSource = %v, want %v", src, want)
	}
	if got := gitT(t, dir, "symbolic-ref", "--short", "refs/remotes/pub/HEAD"); got != "pub/trunk" {
		t.Errorf("after fetching, pub/HEAD is %s, want pub/trunk", got)
	}
	if err := r.addWorktree("y", src); err != nil {
		t.Fatal(err)
	}
	if got := gitT(t, dir+"@y", "rev-parse", "HEAD"); got != head {
		t.Errorf("worktree y is at %s, want %s", got, head)
	}

	// Having chosen a new branch over pub/z, the user is not asked
	// again; without having chosen, confirm would ask.
	gitT(t, pub, "branch", "z")
	src, err = fetchSource(r, "", BranchSource{branch: "z", isNew: true, fetch: "pub", from: "pub/trunk"}, true)
	if err != nil || !src.isNew || src.remote != "" {
		t.Errorf("fetchSource of new branch z, offered = %v, %v, want a new branch", src, err)
	}
}

func TestBranchSourceText(t *testing.T) {
	srcs := []BranchSource{
		{branch: "team/x", hash: "abc1234"},
		{branch: "feature/x", remote: "origin/feature/x", hash: "abc1234"},
		{branch: "x", isNew: true, from: "main", hash: "abc1234", note: "default branch"},
		{branch: "x", isNew: true, hash: "abc1234", note: "checked out"},
		{branch: "x", isNew: true, fetch: "origin", from: "origin/main", note: "default branch on origin"},
		{branch: "x", isNew: true},
	}
	wantStrings := []string{
		"branch team/x (abc1234)",
		"new branch feature/x tracking origin/feature/x (abc1234)",
		"new branch x from main (abc1234), default branch",
		"new branch x from abc1234, checked out",
		"new branch x from origin/main after fetching, default branch on origin",
		"new branch x",
	}
	for i, s := range srcs {
		if got := s.String(); got != wantStrings[i] {
			t.Errorf("String() = %q, want %q", got, wantStrings[i])
		}
	}
	wantRows := []string{
		"<b>team/x</b>      existing\n            <span fgalpha=\"55%\">abc1234</span>   ",
		"<b>feature/x</b>   new, tracking origin/feature/x\n            <span fgalpha=\"55%\">abc1234</span>   ",
		"<b>x</b>           new from main\n            <span fgalpha=\"55%\">abc1234</span>   <i><span foreground=\"#009c8f\">default branch</span></i>",
		"<b>x</b>           new from HEAD\n            <span fgalpha=\"55%\">abc1234</span>   <i><span foreground=\"#009c8f\">checked out</span></i>",
		"<b>x</b>           new from origin/main after fetching\n            <i><span foreground=\"#009c8f\">default branch on origin</span></i>",
		"<b>x</b>           new from HEAD\n            ",
	}
	if got := branchRows(srcs, "#009c8f"); !slices.Equal(got, wantRows) {
		t.Errorf("branchRows =\n%s\nwant\n%s", strings.Join(got, "\n"), strings.Join(wantRows, "\n"))
	}
}
