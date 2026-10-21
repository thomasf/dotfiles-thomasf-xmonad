//go:build linux

package main

import (
	"cmp"
	"errors"
	"fmt"
	"html"
	"os"
	"path/filepath"
	"slices"
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
		fmt.Fprint(os.Stderr, wtUsage)
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
	if err := addWorktree(main, name, exactBranch(main, cmp.Or(*branch, argBranch))); err != nil {
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

	msg := fmt.Sprintf("Worktree %s\n  repository  %s\n  directory   %s", w.prefix(), main, dir)
	var src branchSource
	switch {
	case yes:
		src = branchSource{branch: name, isNew: true}
		if len(srcs) > 0 {
			src = srcs[0]
		}
	case len(srcs) > 0 && srcs[0].branch == name:
		src = srcs[0]
		if !confirm(msg+"\n  branch      "+src.String(), "create") {
			return errCancelled
		}
	default:
		// Branches only resemble the name, or a new branch can start
		// from several places: choosing one is the confirmation.
		news, def, err := newBranches(main, name)
		if err != nil {
			return err
		}
		options := append(srcs, news...)
		var lines []string
		for _, s := range options {
			lines = append(lines, s.String())
		}
		i, err := chooseRows(msg+"\nCreate on which branch?", lines, branchRows(options, accentColour()), len(srcs)+def)
		if err != nil {
			return err
		}
		src = options[i]
	}

	var id string // of the notification to replace
	if src.fetch != "" {
		id = notify("", false, "fetching "+src.fetch+" for "+w.prefix())
		src, err = fetchSource(main, msg, src)
	}
	if err == nil {
		err = addWorktree(main, name, src)
	}
	if err != nil {
		if !errors.Is(err, errCancelled) {
			notify(id, true, "creating worktree "+w.prefix()+" failed: "+err.Error())
		}
		return err
	}
	src.note = ""
	notify(id, false, "created "+w.prefix()+": "+src.String())
	return nil
}

// fetchSource fetches remote src.fetch of the repository of main
// checkout main and returns src, new branch src.branch, to start from
// the default branch of the remote as fetched. If src.branch is on the
// remote now, it offers to check out that instead; if fetching fails,
// to start from the default branch as last fetched. msg describes the
// worktree, as initWorktree shows it.
func fetchSource(main, msg string, src branchSource) (branchSource, error) {
	r := src.fetch
	src.fetch = ""
	if err := fetchRemote(main, r); err != nil {
		if _, herr := git(main, "rev-parse", "--verify", "--quiet", "refs/remotes/"+r+"/HEAD"); herr != nil {
			return src, err
		}
		if !confirm(msg+"\n"+err.Error(), "create from "+src.from+" as last fetched") {
			return src, errCancelled
		}
		return src, nil
	}
	if h, err := git(main, "rev-parse", "--verify", "--quiet", "--short", "refs/remotes/"+r+"/"+src.branch); err == nil {
		tracked := branchSource{branch: src.branch, remote: r + "/" + src.branch, hash: h}
		if !confirm(msg+"\nBranch "+src.branch+" is on "+r+" now.", "create on "+tracked.String()) {
			return src, errCancelled
		}
		return tracked, nil
	}
	// The default branch may have changed with the fetch.
	src.from = cmp.Or(remoteHead(main, r), r+"/HEAD")
	src.hash, _ = git(main, "rev-parse", "--short", src.from)
	return src, nil
}

// fetchRemote fetches remote r of the repository of main checkout
// main, and updates r/HEAD to the default branch of the remote, which
// may have changed since the clone.
func fetchRemote(main, r string) error {
	_, err := git(main, "-c", "remote."+r+".followRemoteHEAD=always", "fetch", "--quiet", r)
	return err
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
func newBranches(main, name string) (srcs []branchSource, def int, err error) {
	hash, err := git(main, "rev-parse", "--short", "HEAD")
	if err != nil {
		return nil, 0, err
	}
	current, _ := git(main, "branch", "--show-current")
	short := func(ref string) string {
		h, _ := git(main, "rev-parse", "--short", ref)
		return h
	}
	remotes := sortRemotes(main)
	for _, r := range remotes {
		from := cmp.Or(remoteHead(main, r), r+"/HEAD")
		srcs = append(srcs, branchSource{branch: name, isNew: true, fetch: r, from: from, note: "default branch on " + r})
	}
	var remote string
	if len(remotes) > 0 {
		remote = remoteHead(main, remotes[0])
	}
	local := defaultLocalBranch(main, remote)
	if local != "" {
		note := "default branch"
		if local == current {
			note += ", checked out"
		}
		if remote != "" {
			ahead, behind := aheadBehind(main, local, remote)
			switch r := remotes[0]; {
			case ahead > 0 && behind > 0:
				// Not "behind": after an amend or a rebase, what
				// only the remote has is the old version.
				note += ", " + commits(ahead) + " not on " + r + ", " + r + " has " + strconv.Itoa(behind) + " not here"
			case ahead > 0:
				note += ", " + commits(ahead) + " not on " + r
			case behind > 0:
				note += ", " + commits(behind) + " behind " + r + " as last fetched"
			}
			if ahead > 0 {
				def = len(srcs)
			}
		}
		srcs = append(srcs, branchSource{branch: name, isNew: true, from: local, hash: short(local), note: note})
	}
	for _, r := range remotes {
		if b := remoteHead(main, r); b != "" {
			note := "default branch on " + r
			if age := refAge(main, b); age != "" {
				note += ", updated " + age
			}
			srcs = append(srcs, branchSource{branch: name, isNew: true, from: b, hash: short(b), note: note})
		}
	}
	if current != local || current == "" {
		// current is "" when the HEAD is detached: the branch then
		// starts from the HEAD, shown by its hash.
		srcs = append(srcs, branchSource{branch: name, isNew: true, from: current, hash: hash, note: "checked out"})
	}
	return srcs, def, nil
}

// aheadBehind returns the number of commits that ref a of the
// repository of main checkout main has and ref b does not, and the
// other way round. It follows the history, not the dates, so an amended
// or rebased commit counts on both sides.
func aheadBehind(main, a, b string) (ahead, behind int) {
	out, err := git(main, "rev-list", "--left-right", "--count", a+"..."+b)
	if err != nil {
		return 0, 0
	}
	l, r, _ := strings.Cut(out, "\t")
	ahead, _ = strconv.Atoi(l)
	behind, _ = strconv.Atoi(r)
	return ahead, behind
}

// commits returns "1 commit" or "<n> commits".
func commits(n int) string {
	if n == 1 {
		return "1 commit"
	}
	return strconv.Itoa(n) + " commits"
}

// sortRemotes returns the remotes of the repository of main checkout
// main, the one the project follows first: the remote whose default
// branch a local branch of the same name tracks, else upstream, which
// a fork names the project it is forked from, else origin. Remotes
// tie in that order.
func sortRemotes(main string) []string {
	out, _ := git(main, "remote")
	remotes := strings.Fields(out)
	rank := func(r string) int {
		switch r {
		case "upstream":
			return 0
		case "origin":
			return 1
		}
		return 2
	}
	slices.SortStableFunc(remotes, func(a, b string) int { return cmp.Compare(rank(a), rank(b)) })
	for i, r := range remotes {
		b, ok := strings.CutPrefix(remoteHead(main, r), r+"/")
		if !ok {
			continue
		}
		if tracked, _ := git(main, "config", "branch."+b+".remote"); tracked == r {
			// Move r to the front.
			copy(remotes[1:i+1], remotes[:i])
			remotes[0] = r
			break
		}
	}
	return remotes
}

// refAge returns how long ago remote-tracking branch ref, like
// origin/main, last changed, like "2 days ago", or "" if its reflog
// does not say. A fetch that brings nothing new does not change it.
func refAge(main, ref string) string {
	out, _ := git(main, "log", "-g", "-1", "--date=relative", "--format=%gd", "refs/remotes/"+ref)
	_, age, ok := strings.Cut(out, "@{")
	if !ok {
		return ""
	}
	return strings.TrimSuffix(age, "}")
}

// remoteHead returns the default branch of remote r of the repository
// of main checkout main, like origin/main, as its remote-tracking branch
// r/HEAD points to it when last fetched, or "" if that is not known.
func remoteHead(main, r string) string {
	b, _ := git(main, "symbolic-ref", "--short", "refs/remotes/"+r+"/HEAD")
	return b
}

// defaultLocalBranch returns the default local branch of the repository
// of main checkout main, or "" if there is none: the local branch of
// remote default branch remote, like origin/main, else the branch
// init.defaultBranch names, else main or master.
func defaultLocalBranch(main, remote string) string {
	var names []string
	if _, b, ok := strings.Cut(remote, "/"); ok {
		// A remote name has no "/" when it is the remote of a
		// branch, as it then is for git config branch.<b>.remote.
		names = append(names, b)
	}
	if b, err := git(main, "config", "init.defaultBranch"); err == nil {
		names = append(names, b)
	}
	for _, b := range append(names, "main", "master") {
		if _, err := git(main, "rev-parse", "--verify", "--quiet", "refs/heads/"+b); err == nil {
			return b
		}
	}
	return ""
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
	isNew  bool   // whether the branch is created
	from   string // for a new branch, the branch it starts from, or "" for the HEAD of the main checkout
	fetch  string // for a new branch, the remote to fetch first
	hash   string // the commit checked out, abbreviated, or "" if not known
	note   string // shown to the user after the rest, like "default branch"
}

func (b branchSource) String() string {
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

// branchRows returns srcs as rofi rows in Pango markup, of two lines:
// the branch checked out and how, then its commit, if known, and the
// note, in colour. The columns line up, as rofi's font is monospace.
func branchRows(srcs []branchSource, colour string) []string {
	var branchWidth, hashWidth int
	for _, s := range srcs {
		branchWidth = max(branchWidth, utf8.RuneCountInString(s.branch))
		hashWidth = max(hashWidth, utf8.RuneCountInString(s.hash))
	}
	pad := func(s string, w int) string {
		return strings.Repeat(" ", w-utf8.RuneCountInString(s))
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
		row := "<b>" + html.EscapeString(s.branch) + "</b>" + pad(s.branch, branchWidth) + "   " + html.EscapeString(how) +
			"\n" + strings.Repeat(" ", branchWidth+3)
		if s.hash != "" {
			row += `<span fgalpha="55%">` + html.EscapeString(s.hash) + "</span>" + pad(s.hash, hashWidth) + "   "
		}
		if s.note != "" {
			row += `<i><span foreground="` + colour + `">` + html.EscapeString(s.note) + "</span></i>"
		}
		rows[i] = row
	}
	return rows
}

// matchingBranches returns the branches of the repository of main
// checkout main that match worktree name: those named name, then those
// named name once each "/" is "-", or ending in "/" + name. A branch
// that is both local and on a remote is returned once, as local; one
// on several remotes is returned once for each.
func matchingBranches(main, name string) ([]branchSource, error) {
	out, err := git(main, "for-each-ref", "--format=%(refname) %(objectname:short)", "refs/heads", "refs/remotes")
	if err != nil {
		return nil, err
	}
	refs := strings.Fields(out)
	local := make(map[string]bool)
	for i := 0; i+1 < len(refs); i += 2 {
		if b, ok := strings.CutPrefix(refs[i], "refs/heads/"); ok {
			local[b] = true
		}
	}
	var exact, other []branchSource
	for i := 0; i+1 < len(refs); i += 2 {
		ref, hash := refs[i], refs[i+1]
		var src branchSource
		if b, ok := strings.CutPrefix(ref, "refs/heads/"); ok {
			src = branchSource{branch: b, hash: hash}
		} else {
			remote := strings.TrimPrefix(ref, "refs/remotes/")
			_, b, ok := strings.Cut(remote, "/")
			if !ok || b == "HEAD" || local[b] {
				continue
			}
			src = branchSource{branch: b, remote: remote, hash: hash}
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
	case src.isNew && src.from != "":
		args = append(args, "--no-track", "-b", src.branch, dir, src.from)
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
