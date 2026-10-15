//go:build linux

package main

import (
	"cmp"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

func runWt(args []string) error {
	if len(args) == 0 {
		usage()
	}
	switch cmd, args := args[0], args[1:]; cmd {
	case "list":
		return runWtList(args)
	case "add":
		return runWtAdd(args)
	case "init":
		return runWtInit(args)
	case "remove", "rm":
		return runWtRemove(args)
	}
	usage()
	return nil
}

func runWtList(args []string) error {
	fs := flag.NewFlagSet("wt list", flag.ExitOnError)
	long := fs.Bool("l", false, "also print the directory and branch of each worktree")
	pick := fs.Bool("pick", false, "choose with rofi between repositories with the same name")
	fs.Parse(args)
	w, err := workspaceArg(fs.Args())
	if err != nil {
		return err
	}
	main, err := projectCheckout(w.project(), *pick)
	if err != nil {
		return err
	}
	wts, err := listWorktrees(w.project(), main)
	if err != nil {
		return err
	}
	for _, wt := range wts {
		if *long {
			fmt.Printf("%s\t%s\t%s\n", wt.prefix, wt.dir, wt.branch)
		} else {
			fmt.Println(wt.prefix)
		}
	}
	return nil
}

func runWtAdd(args []string) error {
	fs := flag.NewFlagSet("wt add", flag.ExitOnError)
	branch := fs.String("branch", "", "check out `branch` instead of the branch named like the worktree")
	pick := fs.Bool("pick", false, "choose with rofi between repositories with the same name")
	fs.Parse(args)
	if fs.NArg() != 1 {
		usage()
	}
	project, name, err := worktreeArg(fs.Arg(0))
	if err != nil {
		return err
	}
	main, err := projectCheckout(project, *pick)
	if err != nil {
		return err
	}
	if err := addWorktree(main, name, exactBranch(main, cmp.Or(*branch, name))); err != nil {
		return err
	}
	fmt.Println(project + "@" + name)
	return nil
}

func runWtRemove(args []string) error {
	fs := flag.NewFlagSet("wt remove", flag.ExitOnError)
	force := fs.Bool("force", false, "remove the worktree even if it has changes")
	yes := fs.Bool("y", false, "remove the worktree without asking")
	pick := fs.Bool("pick", false, "choose with rofi between repositories with the same name")
	fs.Parse(args)
	var project, name string
	switch fs.NArg() {
	case 0:
		w, err := currentWorkspace()
		if err != nil {
			return err
		}
		project, name = w.project(), w.worktree()
		if name == "" {
			return fmt.Errorf("workspace %s is not in a worktree", w)
		}
	case 1:
		var err error
		if project, name, err = worktreeArg(fs.Arg(0)); err != nil {
			return err
		}
	default:
		usage()
	}
	main, err := projectCheckout(project, *pick)
	if err != nil {
		return err
	}
	if !*yes {
		ok, discard, err := confirmRemove(main, project+"@"+name, name)
		if err != nil {
			return err
		}
		if !ok {
			return errCancelled
		}
		*force = *force || discard
	}
	// The branch is kept; look it up now, as the worktree is gone after.
	branch, _ := git(worktreePath(main, name), "branch", "--show-current")
	if err := removeWorktree(main, name, *force); err != nil {
		return err
	}
	if err := storeDir(project+"@"+name, ""); err != nil {
		return err
	}
	if branch != "" {
		fmt.Printf("\nBranch %s was kept. To delete it:\n\n  git -C %s branch -d %s\n\n", branch, main, branch)
	}
	return nil
}

// confirmRemove asks the user whether to remove worktree name, with
// workspace prefix prefix, of the main checkout main. If the worktree
// has changes, agreeing means discarding them too.
func confirmRemove(main, prefix, name string) (ok, discard bool, err error) {
	dir := worktreePath(main, name)
	branch, err := git(dir, "branch", "--show-current")
	if err != nil {
		return false, false, err
	}
	status, err := git(dir, "status", "--porcelain")
	if err != nil {
		return false, false, err
	}
	msg := fmt.Sprintf("Worktree %s\n  directory   %s\n  branch      %s (kept)", prefix, dir, cmp.Or(branch, "(detached)"))
	if status == "" {
		return confirm(msg, "remove"), false, nil
	}
	msg += fmt.Sprintf("\n  changes     %d files", len(strings.Split(status, "\n")))
	return confirm(msg, "remove, discarding changes"), true, nil
}

func runWtInit(args []string) error {
	fs := flag.NewFlagSet("wt init", flag.ExitOnError)
	yes := fs.Bool("y", false, "create the worktree without asking, on the first branch found")
	pick := fs.Bool("pick", false, "choose with rofi between repositories with the same name")
	fs.Parse(args)
	w, err := workspaceArg(fs.Args())
	if err != nil {
		return err
	}
	if err := initWorktree(w, *yes, *pick); err != nil {
		return err
	}
	fmt.Println(w.prefix())
	return nil
}

// initWorktree creates the worktree of workspace w, as ws wt init does.
// It returns errCancelled if the user does not want it.
func initWorktree(w workspace, yes, pick bool) error {
	name := w.worktree()
	if name == "" {
		return fmt.Errorf("workspace %s is not in a worktree: name it <project>@<worktree>", w)
	}
	if !validWorktreeName(name) {
		return fmt.Errorf("bad worktree name %q", name)
	}
	main, err := projectCheckout(w.project(), pick)
	if err != nil {
		return err
	}
	dir := worktreePath(main, name)
	if _, err := os.Stat(dir); err == nil {
		return fmt.Errorf("worktree %s exists: %s", w.prefix(), dir)
	}
	srcs, err := matchingBranches(main, name)
	if err != nil {
		return err
	}
	base, err := git(main, "log", "-1", "--format=%D %h")
	if err != nil {
		return err
	}
	src := branchSource{branch: name, isNew: true, base: describeHead(base)}

	msg := fmt.Sprintf("Worktree %s\n  repository  %s\n  directory   %s", w.prefix(), main, dir)
	switch {
	case yes:
		if len(srcs) > 0 {
			src = srcs[0]
		}
	case len(srcs) > 0 && srcs[0].branch == name, len(srcs) == 0:
		if len(srcs) > 0 {
			src = srcs[0]
		}
		if !confirm(msg+"\n  branch      "+src.String(), "create") {
			return errCancelled
		}
	default:
		// Branches only resemble the name: choosing one is the
		// confirmation.
		var options []string
		for _, s := range srcs {
			options = append(options, s.String())
		}
		i, err := choose(msg+"\nCreate on which branch?", append(options, src.String()))
		if err != nil {
			return err
		}
		if i < len(srcs) {
			src = srcs[i]
		}
	}
	return addWorktree(main, name, src)
}

// describeHead returns the HEAD of a checkout as the user knows it, from
// the output of git log --format="%D %h": its branch, or its commit when
// it is detached.
func describeHead(log string) string {
	refs, hash, ok := strings.CutLast(log, " ")
	if !ok {
		return log
	}
	if b, ok := strings.CutPrefix(refs, "HEAD -> "); ok {
		b, _, _ = strings.Cut(b, ",")
		return b + " (" + hash + ")"
	}
	return hash
}

// worktreeArg returns the project and worktree name in arg,
// [project@]name. Without a project, it is the project of the current
// workspace.
func worktreeArg(arg string) (project, name string, err error) {
	project, name, ok := strings.Cut(arg, "@")
	if !ok {
		w, err := currentWorkspace()
		if err != nil {
			return "", "", err
		}
		project, name = w.project(), arg
	}
	if !validWorktreeName(name) {
		return "", "", fmt.Errorf("bad worktree name %q", name)
	}
	return project, name, nil
}

// validWorktreeName reports whether name can be the worktree in a
// workspace name and a directory name.
func validWorktreeName(name string) bool {
	return name != "" && !strings.HasPrefix(name, "-") && !strings.ContainsAny(name, "./@ \t\n")
}

// projectCheckout returns the main checkout of the repository of
// project.
func projectCheckout(project string, pick bool) (string, error) {
	p := noPick
	if pick {
		p = rofiPick
	}
	dir, err := projectDir(project, p)
	if err != nil {
		return "", err
	}
	main, _, err := mainCheckout(dir)
	return main, err
}

// A worktree is a checkout of a project's repository.
type worktree struct {
	prefix string // workspace prefix, <project>[@<name>]
	dir    string
	branch string // short branch name, or "(detached)"
}

// listWorktrees returns the main checkout main of project and its
// worktrees named <repo>@<name> beside it. It leaves out other
// worktrees and those whose directory is gone.
func listWorktrees(project, main string) ([]worktree, error) {
	out, err := git(main, "worktree", "list", "--porcelain")
	if err != nil {
		return nil, err
	}
	var wts []worktree
	for block := range strings.SplitSeq(out, "\n\n") {
		var wt worktree
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
		if wt.dir == main {
			wt.prefix = project
		} else if filepath.Dir(wt.dir) == filepath.Dir(main) {
			name, ok := strings.CutPrefix(filepath.Base(wt.dir), filepath.Base(main)+"@")
			if !ok || !validWorktreeName(name) {
				continue
			}
			wt.prefix = project + "@" + name
		} else {
			continue
		}
		wts = append(wts, wt)
	}
	return wts, nil
}

// A branchSource is the branch a new worktree checks out.
type branchSource struct {
	branch string // local branch
	remote string // remote-tracking branch, like origin/x, that a new branch tracks
	isNew  bool   // whether the branch is created from the HEAD of the main checkout
	base   string // for a new branch, what HEAD is, as shown to the user
}

func (b branchSource) String() string {
	switch {
	case b.isNew && b.base != "":
		return "new branch " + b.branch + " from " + b.base
	case b.isNew:
		return "new branch " + b.branch
	case b.remote != "":
		return "new branch " + b.branch + " tracking " + b.remote
	}
	return "branch " + b.branch
}

// matchingBranches returns the branches of the repository of main
// checkout main that match worktree name: those named name, then those
// named name once each "/" is "-", or ending in "/" + name. A branch
// that is both local and on a remote is returned once, as local; one
// on several remotes is returned once for each.
func matchingBranches(main, name string) ([]branchSource, error) {
	out, err := git(main, "for-each-ref", "--format=%(refname)", "refs/heads", "refs/remotes")
	if err != nil {
		return nil, err
	}
	refs := strings.Fields(out)
	local := make(map[string]bool)
	for _, ref := range refs {
		if b, ok := strings.CutPrefix(ref, "refs/heads/"); ok {
			local[b] = true
		}
	}
	var exact, other []branchSource
	for _, ref := range refs {
		var src branchSource
		if b, ok := strings.CutPrefix(ref, "refs/heads/"); ok {
			src = branchSource{branch: b}
		} else {
			remote := strings.TrimPrefix(ref, "refs/remotes/")
			_, b, ok := strings.Cut(remote, "/")
			if !ok || b == "HEAD" || local[b] {
				continue
			}
			src = branchSource{branch: b, remote: remote}
		}
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
func exactBranch(main, branch string) branchSource {
	srcs, _ := matchingBranches(main, branch)
	if len(srcs) > 0 && srcs[0].branch == branch {
		return srcs[0]
	}
	return branchSource{branch: branch, isNew: true}
}

// addWorktree adds worktree name to the main checkout main, checking out
// src.
func addWorktree(main, name string, src branchSource) error {
	dir := worktreePath(main, name)
	args := []string{"worktree", "add", "--quiet"}
	switch {
	case src.isNew:
		args = append(args, "-b", src.branch, dir)
	case src.remote != "":
		args = append(args, "--track", "-b", src.branch, dir, src.remote)
	default:
		args = append(args, dir, src.branch)
	}
	_, err := git(main, args...)
	return err
}

// removeWorktree removes worktree name of the main checkout main. Git
// refuses if it has changes, unless force is set. Its branch is kept.
func removeWorktree(main, name string, force bool) error {
	args := []string{"worktree", "remove"}
	if force {
		args = append(args, "--force")
	}
	_, err := git(main, append(args, worktreePath(main, name))...)
	return err
}
