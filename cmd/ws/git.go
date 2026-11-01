//go:build linux

package main

import (
	"bytes"
	"cmp"
	"errors"
	"fmt"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
)

// A Repository is the checkout directory of a Git repository.
type Repository string

func (r Repository) git(args ...string) (string, error) {
	out, err := exec.Command("git", append([]string{"-C", string(r)}, args...)...).Output()
	if ee, ok := errors.AsType[*exec.ExitError](err); ok && len(ee.Stderr) > 0 {
		return "", fmt.Errorf("git %s in %s: %s", strings.Join(args, " "), r, bytes.TrimSpace(ee.Stderr))
	}
	if err != nil {
		return "", fmt.Errorf("git %s in %s: %w", strings.Join(args, " "), r, err)
	}
	return strings.TrimSpace(string(out)), nil
}

func (r Repository) currentBranch() (string, error) {
	return r.git("branch", "--show-current")
}

func (r Repository) shortCommit(ref string) string {
	h, _ := r.git("rev-parse", "--short", ref)
	return h
}

func (r Repository) refExists(ref string) bool {
	_, err := r.git("rev-parse", "--verify", "--quiet", ref)
	return err == nil
}

// mainCheckout returns the main checkout of the repository that dir
// is in, and the path of dir relative to the checkout it is in, which
// may be a worktree.
func mainCheckout(dir string) (main Repository, rel string, err error) {
	dir, err = filepath.EvalSymlinks(dir)
	if err != nil {
		return "", "", err
	}
	top, err := Repository(dir).git("rev-parse", "--show-toplevel")
	if err != nil {
		return "", "", err
	}
	common, err := Repository(dir).git("rev-parse", "--path-format=absolute", "--git-common-dir")
	if err != nil {
		return "", "", err
	}
	if filepath.Base(common) != ".git" {
		return "", "", fmt.Errorf("%s: no main checkout", dir)
	}
	rel, err = filepath.Rel(top, dir)
	return Repository(filepath.Dir(common)), rel, err
}

// worktreePath returns the directory of worktree name of the main checkout r.
func (r Repository) worktreePath(name string) string {
	return filepath.Join(filepath.Dir(string(r)), filepath.Base(string(r))+"@"+name)
}

// parseWorktreeDir checks whether dir is a worktree sibling of r and returns
// its worktree name.
func (r Repository) parseWorktreeDir(dir string) (name string, ok bool) {
	if filepath.Dir(dir) != filepath.Dir(string(r)) {
		return "", false
	}
	name, ok = strings.CutPrefix(filepath.Base(dir), filepath.Base(string(r))+"@")
	return name, ok && validWorktreeName(name)
}

// A WorkTree is a checkout of a project's repository.
type WorkTree struct {
	prefix string // workspace prefix, <project>[@<name>]
	dir    string
	branch string // short branch name, or "(detached)"
}

// listWorktrees returns the main checkout r of project and its
// worktrees named <repo>@<name> beside it. It leaves out other
// worktrees and those whose directory is gone.
func (r Repository) listWorktrees(project string) ([]WorkTree, error) {
	out, err := r.git("worktree", "list", "--porcelain")
	if err != nil {
		return nil, err
	}
	var wts []WorkTree
	for block := range strings.SplitSeq(out, "\n\n") {
		var wt WorkTree
		prunable := false
		for line := range strings.SplitSeq(block, "\n") {
			switch k, v, _ := strings.Cut(line, " "); k {
			case "worktree":
				wt.dir = v
			case "branch":
				wt.branch = strings.TrimPrefix(v, "refs/heads/")
			case "detached":
				wt.branch = "(detached)"
			case "prunable":
				prunable = true
			}
		}
		if prunable {
			continue
		}
		if wt.dir == string(r) {
			wt.prefix = project
		} else if name, ok := r.parseWorktreeDir(wt.dir); ok {
			wt.prefix = project + "@" + name
		} else {
			continue
		}
		wts = append(wts, wt)
	}
	return wts, nil
}

// A BranchSource is the branch a new worktree checks out.
type BranchSource struct {
	branch string // local branch
	remote string // remote-tracking branch, like origin/x, that a new branch tracks
	isNew  bool   // whether the branch is created
	from   string // for a new branch, the branch it starts from, or "" for the HEAD of the main checkout
	fetch  string // for a new branch, the remote to fetch first
	hash   string // the commit checked out, abbreviated, or "" if not known
	note   string // shown to the user after the rest, like "default branch"
}

func (b BranchSource) String() string {
	var s string
	switch {
	case b.isNew:
		s = "new branch " + b.branch
		if from := cmp.Or(b.from, b.hash); from != "" {
			s += " from " + from
		}
		if b.fetch != "" {
			s += " after fetching"
		}
	case b.remote != "":
		s = "new branch " + b.branch + " tracking " + b.remote
	default:
		s = "branch " + b.branch
	}
	if b.hash != "" && (b.from != "" || !b.isNew) {
		s += " (" + b.hash + ")"
	}
	if b.note != "" {
		s += ", " + b.note
	}
	return s
}

// A BranchInfo is a branch with its last commit.
type BranchInfo struct {
	BranchSource
	age     string // how long ago the commit was made, like "2 days ago"
	author  string
	subject string
}

// branches returns the branches of the repository of checkout r,
// sorted by key as git for-each-ref --sort takes it: those local,
// and those only on a remote, once for each remote.
func (r Repository) branches(key string) ([]BranchInfo, error) {
	out, err := r.git("for-each-ref", "--sort="+key,
		"--format=%(refname)%00%(objectname:short)%00%(committerdate:relative)%00%(authorname)%00%(subject)",
		"refs/heads", "refs/remotes")
	if err != nil || out == "" {
		return nil, err
	}
	var refs [][]string
	local := make(map[string]bool)
	for line := range strings.SplitSeq(out, "\n") {
		f := strings.SplitN(line, "\x00", 5)
		if len(f) < 5 {
			continue
		}
		refs = append(refs, f)
		if b, ok := strings.CutPrefix(f[0], "refs/heads/"); ok {
			local[b] = true
		}
	}
	var infos []BranchInfo
	for _, f := range refs {
		info := BranchInfo{BranchSource{hash: f[1]}, f[2], f[3], f[4]}
		if b, ok := strings.CutPrefix(f[0], "refs/heads/"); ok {
			info.branch = b
		} else {
			remote := strings.TrimPrefix(f[0], "refs/remotes/")
			_, b, ok := strings.Cut(remote, "/")
			if !ok || b == "HEAD" || local[b] {
				continue
			}
			info.branch, info.remote = b, remote
		}
		infos = append(infos, info)
	}
	return infos, nil
}

// matchingBranches returns the branches of the repository of checkout r
// that match worktree name: those named name, then those named name once each
// "/" is "-", or ending in "/" + name. A branch that is both local and on a remote
// is returned once, as local; one on several remotes is returned once for each.
func (r Repository) matchingBranches(name string) ([]BranchSource, error) {
	infos, err := r.branches("refname")
	if err != nil {
		return nil, err
	}
	var exact, other []BranchSource
	for _, b := range infos {
		src := b.BranchSource
		switch {
		case src.branch == name:
			exact = append(exact, src)
		case strings.ReplaceAll(src.branch, "/", "-") == name, strings.HasSuffix(src.branch, "/"+name):
			other = append(other, src)
		}
	}
	return append(exact, other...), nil
}

// exactBranch returns the branch named branch, local or else on a
// remote, or a new branch if there is none.
func (r Repository) exactBranch(branch string) BranchSource {
	srcs, _ := r.matchingBranches(branch)
	if len(srcs) > 0 && srcs[0].branch == branch {
		return srcs[0]
	}
	return BranchSource{branch: branch, isNew: true}
}

// addWorktree adds worktree name to checkout r, checking out src.
func (r Repository) addWorktree(name string, src BranchSource) error {
	dir := r.worktreePath(name)
	args := []string{"worktree", "add", "--quiet"}
	switch {
	case src.isNew && src.from != "":
		args = append(args, "--no-track", "-b", src.branch, dir, src.from)
	case src.isNew:
		args = append(args, "-b", src.branch, dir)
	case src.remote != "":
		args = append(args, "--track", "-b", src.branch, dir, src.remote)
	default:
		args = append(args, dir, src.branch)
	}
	_, err := r.git(args...)
	return err
}

// removeWorktree removes worktree name of checkout r. Git
// refuses if it has changes, unless force is set. Its branch is kept.
func (r Repository) removeWorktree(name string, force bool) error {
	args := []string{"worktree", "remove"}
	if force {
		args = append(args, "--force")
	}
	_, err := r.git(append(args, r.worktreePath(name))...)
	return err
}

func (r Repository) fetchRemote(remote string) error {
	_, err := r.git("-c", "remote."+remote+".followRemoteHEAD=always", "fetch", "--quiet", remote)
	return err
}

func (r Repository) remoteHead(remote string) string {
	b, _ := r.git("symbolic-ref", "--short", "refs/remotes/"+remote+"/HEAD")
	return b
}

func (r Repository) refAge(ref string) string {
	out, _ := r.git("log", "-g", "-1", "--date=relative", "--format=%gd", "refs/remotes/"+ref)
	_, age, ok := strings.Cut(out, "@{")
	if !ok {
		return ""
	}
	return strings.TrimSuffix(age, "}")
}

func (r Repository) aheadBehind(a, b string) (ahead, behind int) {
	out, err := r.git("rev-list", "--left-right", "--count", a+"..."+b)
	if err != nil {
		return 0, 0
	}
	l, rStr, _ := strings.Cut(out, "\t")
	ahead, _ = strconv.Atoi(l)
	behind, _ = strconv.Atoi(rStr)
	return ahead, behind
}

func formatCommits(n int) string {
	if n == 1 {
		return "1 commit"
	}
	return strconv.Itoa(n) + " commits"
}

func (r Repository) sortRemotes() []string {
	out, _ := r.git("remote")
	remotes := strings.Fields(out)
	rank := func(remote string) int {
		switch remote {
		case "upstream":
			return 0
		case "origin":
			return 1
		}
		return 2
	}
	slices.SortStableFunc(remotes, func(a, b string) int { return cmp.Compare(rank(a), rank(b)) })
	for i, remote := range remotes {
		b, ok := strings.CutPrefix(r.remoteHead(remote), remote+"/")
		if !ok {
			continue
		}
		if tracked, _ := r.git("config", "branch."+b+".remote"); tracked == remote {
			copy(remotes[1:i+1], remotes[:i])
			remotes[0] = remote
			break
		}
	}
	return remotes
}

func (r Repository) defaultLocalBranch(remote string) string {
	var names []string
	if _, b, ok := strings.Cut(remote, "/"); ok {
		names = append(names, b)
	}
	if b, err := r.git("config", "init.defaultBranch"); err == nil {
		names = append(names, b)
	}
	for _, b := range append(names, "main", "master") {
		if r.refExists("refs/heads/" + b) {
			return b
		}
	}
	return ""
}
