//go:build linux

package main

import (
	"cmp"
	"errors"
	"fmt"
	"html"
	"os"
	"strconv"
	"strings"
	"unicode/utf8"
)

const wtUsage = `usage: ws wt <command> [arguments]

Wt manages the worktrees of a project, by default the project of the
current workspace. Worktree name of a repository is the directory
<repo>@<name> beside its main checkout. A name cannot contain "/", ".",
"@" or spaces: worktree feat-x is the usual name for branch feat/x.

Commands:
    list    print the workspace prefixes of the worktrees
    add     create a worktree on a given branch, without asking
    init    create a worktree, choosing its branch
    remove  remove a worktree (or rm)

Run ws wt <command> -h for help on a command.
`

func runWt(args []string) error {
	if len(args) == 0 {
		fmt.Fprint(os.Stderr, wtUsage)
		os.Exit(2)
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
	case "-h", "-help", "--help", "help":
		fmt.Fprint(os.Stdout, wtUsage)
		os.Exit(0)
	}
	fmt.Fprint(os.Stderr, wtUsage)
	os.Exit(2)
	return nil
}

func runWtList(args []string) error {
	fs := newFlagSet("wt list", "[-l] [-pick] [workspace]", `
List prints the workspace prefix of the main checkout of the project of
a workspace, by default the current one, and of each of its worktrees,
one per line.
`)
	long := fs.Bool("l", false, "also print the directory and branch of each worktree")
	pick := fs.Bool("pick", false, "choose with rofi between repositories with the same name")
	fs.Parse(args)
	w, err := workspaceArg(fs)
	if err != nil {
		return err
	}
	main, err := projectCheckout(w.project(), *pick)
	if err != nil {
		return err
	}
	wts, err := main.listWorktrees(w.project())
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
	fs := newFlagSet("wt add", "[-branch branch] [-pick] [project@]name", `
Add creates the worktree <repo>@<name> beside the main checkout <repo>
of project, by default the project of the current workspace, and prints
its workspace prefix project@name. It never asks. It checks out branch
name, or -branch: the local branch, else the branch on a remote,
tracking it, else a new branch from the HEAD of the main checkout. A
name with "/" is a branch: feat/x checks out branch feat/x in worktree
feat-x.

The project must be the name of a ghq repository, or have a directory
stored with ws dir -set; ws dir project prints the checkout found. The
directory <repo>@<name> must not exist.

Examples:

    ws wt add fix-y                     # branch fix-y, current project
    ws wt add repo@spike                # ../repo@spike on branch spike,
                                        # new from HEAD if there is none
    ws wt add repo@feat/x               # ../repo@feat-x on branch feat/x
    ws wt add -branch feat/x repo@x     # ../repo@x on branch feat/x
`)
	branch := fs.String("branch", "", "check out `branch` instead of the branch named like the worktree")
	pick := fs.Bool("pick", false, "choose with rofi between repositories with the same name")
	fs.Parse(args)
	if fs.NArg() != 1 {
		usageError(fs)
	}
	project, name, argBranch, err := worktreeArg(fs.Arg(0))
	if err != nil {
		return err
	}
	main, err := projectCheckout(project, *pick)
	if err != nil {
		return err
	}
	if err := main.addWorktree(name, main.exactBranch(cmp.Or(*branch, argBranch))); err != nil {
		return err
	}
	fmt.Println(project + "@" + name)
	return nil
}

func runWtRemove(args []string) error {
	fs := newFlagSet("wt remove", "[-force] [-y] [-pick] [[project@]name]", `
Remove removes worktree name, by default the worktree of the current
workspace, and the directory stored for its workspace prefix. The
branch is kept, and it prints the git command to delete it.

It asks first, where init asks, unless -y is given. If the worktree has
changes, agreeing discards them; with -y, git refuses to remove it
unless -force is given.
`)
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
		if project, name, _, err = worktreeArg(fs.Arg(0)); err != nil {
			return err
		}
	default:
		usageError(fs)
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
	branch, _ := Repository(main.worktreePath(name)).currentBranch()
	if err := main.removeWorktree(name, *force); err != nil {
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
func confirmRemove(main Repository, prefix, name string) (ok, discard bool, err error) {
	dir := main.worktreePath(name)
	branch, err := Repository(dir).currentBranch()
	if err != nil {
		return false, false, err
	}
	status, err := Repository(dir).git("status", "--porcelain")
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
	fs := newFlagSet("wt init", "[-y] [-pick] [workspace]", `
Init creates the worktree of a workspace, letting the user choose a
branch resembling its name, or where a new branch starts.

Examples:

    ws wt init                 # worktree of the current workspace
    ws wt init repo@feat-x     # offers feat/x, or a new branch feat-x
    ws wt init repo@x          # offers branches ending in /x, like feat/x
    ws wt init -y repo@feat-x  # first match, else a new branch from HEAD
`)
	yes := fs.Bool("y", false, "create the worktree without asking, on the first branch found")
	pick := fs.Bool("pick", false, "choose with rofi between repositories with the same name")
	fs.Parse(args)
	w, err := workspaceArg(fs)
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
func initWorktree(w Workspace, yes, pick bool) error {
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
	dir := main.worktreePath(name)
	if fileExists(dir) {
		return fmt.Errorf("worktree %s exists: %s", w.prefix(), dir)
	}
	srcs, err := main.matchingBranches(name)
	if err != nil {
		return err
	}

	msg := fmt.Sprintf("Worktree %s\n  repository  %s\n  directory   %s", w.prefix(), main, dir)
	var src BranchSource
	if yes {
		src = BranchSource{branch: name, isNew: true}
		if len(srcs) > 0 {
			src = srcs[0]
		}
	} else {
		// Choosing a branch is the confirmation. New branches are
		// offered too, unless a local branch has the name, as a new
		// one cannot. The cursor is on the branch named name, if there
		// is one, else on the suggested start of a new branch.
		options, selected := srcs, 0
		if len(srcs) == 0 || srcs[0].branch != name || srcs[0].remote != "" {
			news, def, err := newBranches(main, name)
			if err != nil {
				return err
			}
			options = append(srcs, news...)
			if len(srcs) == 0 || srcs[0].branch != name {
				selected = len(srcs) + def
			}
		}
		var lines []string
		for _, s := range options {
			lines = append(lines, s.String())
		}
		i, err := chooseRows(msg+"\nCreate on which branch?", lines, branchRows(options, accentColor()), selected)
		if err != nil {
			return err
		}
		src = options[i]
	}
	// Whether the user was offered the branch named name on a remote,
	// and so chose any new branch over it.
	offered := len(srcs) > 0 && srcs[0].branch == name

	var id string // of the notification to replace
	if src.fetch != "" {
		id = notify("", "fetching "+src.fetch+" for "+w.prefix())
		src, err = fetchSource(main, msg, src, offered)
	}
	if err == nil {
		err = main.addWorktree(name, src)
	}
	if err != nil {
		if !errors.Is(err, errCancelled) {
			notifyError(id, "creating worktree "+w.prefix()+" failed: "+err.Error())
		}
		return err
	}
	src.note = ""
	notify(id, "created "+w.prefix()+": "+src.String())
	return nil
}

// fetchSource fetches remote src.fetch of the repository of main
// checkout main and returns src, new branch src.branch, to start from
// the default branch of the remote as fetched. If src.branch is on the
// remote now, it offers to check out that instead, unless offered says
// the user has chosen src over it already; if fetching fails, to start
// from the default branch as last fetched. msg describes the worktree,
// as initWorktree shows it.
func fetchSource(main Repository, msg string, src BranchSource, offered bool) (BranchSource, error) {
	r := src.fetch
	src.fetch = ""
	if err := main.fetchRemote(r); err != nil {
		if !main.refExists("refs/remotes/" + r + "/HEAD") {
			return src, err
		}
		if !confirm(msg+"\n"+err.Error(), "create from "+src.from+" as last fetched") {
			return src, errCancelled
		}
		return src, nil
	}
	if h, err := main.git("rev-parse", "--verify", "--quiet", "--short", "refs/remotes/"+r+"/"+src.branch); err == nil && !offered {
		tracked := BranchSource{branch: src.branch, remote: r + "/" + src.branch, hash: h}
		if !confirm(msg+"\nBranch "+src.branch+" is on "+r+" now.", "create on "+tracked.String()) {
			return src, errCancelled
		}
		return tracked, nil
	}
	// The default branch may have changed with the fetch.
	src.from = cmp.Or(main.remoteHead(r), r+"/HEAD")
	src.hash = main.shortCommit(src.from)
	return src, nil
}

// newBranches returns the ways to create new branch name in the
// repository of main checkout main: from the default branch of each
// remote, fetched first, then from the local default branch, from the
// default branch of each remote as last fetched, and from the HEAD of
// main. It leaves out those that do not exist, and the HEAD if it is
// the local default branch. The remotes are in the order of
// sortRemotes.
//
// def is the index of the way to suggest: the local default branch if
// it has commits that the default branch of the first remote, as last
// fetched, does not, which a fetch cannot bring; else the first remote
// fetched.
func newBranches(main Repository, name string) (srcs []BranchSource, def int, err error) {
	hash, err := main.git("rev-parse", "--short", "HEAD")
	if err != nil {
		return nil, 0, err
	}
	current, _ := main.currentBranch()
	remotes := main.sortRemotes()
	for _, r := range remotes {
		from := cmp.Or(main.remoteHead(r), r+"/HEAD")
		srcs = append(srcs, BranchSource{branch: name, isNew: true, fetch: r, from: from, note: "default branch on " + r})
	}
	var remote string
	if len(remotes) > 0 {
		remote = main.remoteHead(remotes[0])
	}
	local := main.defaultLocalBranch(remote)
	if local != "" {
		note := "default branch"
		if local == current {
			note += ", checked out"
		}
		if remote != "" {
			ahead, behind := main.aheadBehind(local, remote)
			switch r := remotes[0]; {
			case ahead > 0 && behind > 0:
				// Not "behind": after an amend or a rebase, what
				// only the remote has is the old version.
				note += ", " + formatCommits(ahead) + " not on " + r + ", " + r + " has " + strconv.Itoa(behind) + " not here"
			case ahead > 0:
				note += ", " + formatCommits(ahead) + " not on " + r
			case behind > 0:
				note += ", " + formatCommits(behind) + " behind " + r + " as last fetched"
			}
			if ahead > 0 {
				def = len(srcs)
			}
		}
		srcs = append(srcs, BranchSource{branch: name, isNew: true, from: local, hash: main.shortCommit(local), note: note})
	}
	for _, r := range remotes {
		if b := main.remoteHead(r); b != "" {
			note := "default branch on " + r
			if age := main.refAge(b); age != "" {
				note += ", updated " + age
			}
			srcs = append(srcs, BranchSource{branch: name, isNew: true, from: b, hash: main.shortCommit(b), note: note})
		}
	}
	if current != local || current == "" {
		// current is "" when the HEAD is detached: the branch then
		// starts from the HEAD, shown by its hash.
		srcs = append(srcs, BranchSource{branch: name, isNew: true, from: current, hash: hash, note: "checked out"})
	}
	return srcs, def, nil
}

// worktreeArg returns the project and worktree name in arg,
// [project@]name, and the branch it names. Without a project, it is
// the project of the current workspace. name can be a branch with "/",
// such as feat/x, which names worktree feat-x.
func worktreeArg(arg string) (project, name, branch string, err error) {
	project, branch, ok := strings.Cut(arg, "@")
	if !ok {
		w, err := currentWorkspace()
		if err != nil {
			return "", "", "", err
		}
		project, branch = w.project(), arg
	}
	name = strings.ReplaceAll(branch, "/", "-")
	if !validWorktreeName(name) {
		return "", "", "", fmt.Errorf(`bad worktree name %q: it cannot contain ".", "@" or spaces`, name)
	}
	return project, name, branch, nil
}

// validWorktreeName reports whether name can be the worktree in a
// workspace name and a directory name.
func validWorktreeName(name string) bool {
	return name != "" && !strings.HasPrefix(name, "-") && !strings.ContainsAny(name, "./@ \t\n")
}

// projectCheckout returns the main checkout of the repository of
// project.
func projectCheckout(project string, pick bool) (Repository, error) {
	dir, err := projectDir(project, resolvePicker(pick))
	if err != nil {
		return "", err
	}
	main, _, err := mainCheckout(dir)
	return main, err
}

// branchRows returns srcs as rofi rows in Pango markup, of two lines:
// the branch checked out and how, then its commit, if known, and the
// note, in colour. The columns line up, as rofi's font is monospace.
func branchRows(srcs []BranchSource, colour string) []string {
	var branchWidth, hashWidth int
	for _, s := range srcs {
		branchWidth = max(branchWidth, utf8.RuneCountInString(s.branch))
		hashWidth = max(hashWidth, utf8.RuneCountInString(s.hash))
	}
	rows := make([]string, len(srcs))
	for i, s := range srcs {
		var how string
		switch {
		case s.isNew:
			how = "new from " + cmp.Or(s.from, "HEAD")
			if s.fetch != "" {
				how += " after fetching"
			}
		case s.remote != "":
			how = "new, tracking " + s.remote
		default:
			how = "existing"
		}
		row := pangoBold(s.branch) + padRight(s.branch, branchWidth) + "   " + html.EscapeString(how) +
			"\n" + strings.Repeat(" ", branchWidth+3)
		if s.hash != "" {
			row += pangoDim(s.hash, 55) + padRight(s.hash, hashWidth) + "   "
		}
		if s.note != "" {
			row += "<i>" + pangoColor(s.note, colour) + "</i>"
		}
		rows[i] = row
	}
	return rows
}
