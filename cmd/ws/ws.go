//go:build linux

// Ws handles xmonad workspaces outside xmonad: their names, the
// directories they work in and what is started when one is opened.
//
// Usage:
//
//	ws name [-p | -s]
//	ws dir [-set | -unset] [-pick] [workspace]
//	ws action [workspace]
//	ws wt list [-l] [-pick] [workspace]
//	ws wt add [-branch branch] [-pick] [project@]name
//	ws wt init [-y] [-pick] [workspace]
//	ws wt remove [-force] [-y] [-pick] [[project@]name]
//	ws pick [-go] [-p prompt] [-columns n] [-lines n] [-worktrees]
//	ws go workspace
//	ws rofi [rofi arguments]
//
// A workspace name is <prefix>[.<suffix>], and the prefix is
// <project>[@<worktree>]. In workspace hello@test.bot, the project is
// hello, the worktree test and the suffix bot.
//
// Name prints the name of the current workspace. With -p it prints
// only the prefix; with -s only the suffix, and it exits with status
// 1 if there is none. Output to a terminal ends in a newline; other
// output does not, so that it can be used as it is.
//
// Dir prints the directory a workspace works in, by default the
// current one. It is the directory stored for the prefix with
// -set, if there is one. Else it is the directory stored for the
// project, or the ghq repository named like the project; with -pick,
// rofi chooses between repositories with the same name, and without
// it they are an error. For a worktree, the directory is the
// sibling of the project's main checkout named <repo>@<worktree>,
// at the same place within it. -set stores the working directory for
// the prefix, in ~/.config/wsdirs, and -unset removes it.
//
// Action is run by xmonad when it opens an empty workspace. For a
// workspace in a worktree that does not exist yet, it first offers to
// create it, as wt init does. In the directory of the workspace, if it
// has one, it runs the launcher w.<project> from $PATH. If there is
// none, or it fails, it starts what the suffix calls for:
//
//	www, doc              www-window
//	remote, ssh           sshmenu
//	term, terminal, bot   term
//	emacs                 e
//	files                 nautilus, in the workspace directory
//
// The launchers find the workspace in the environment variables
// WS_NAME, WS_PREFIX, WS_PROJECT, WS_WORKTREE, WS_SUFFIX and WS_DIR.
//
// Wt manages the worktrees of a project, by default the project of the
// current workspace. Worktree name of a repository is the directory
// <repo>@<name> beside its main checkout. List prints the workspace
// prefix of the main checkout and of each such worktree, one per line;
// with -l, followed by its directory and branch. Add creates worktree
// name and prints its workspace prefix. It checks out the branch named
// like the worktree, or the one given with -branch: the local branch,
// or else the branch on a remote, or else a new branch from the HEAD of
// the main checkout. A branch only on a remote is checked out as a new
// branch tracking it.
//
// Init creates the worktree of a workspace <project>@<name>[.<suffix>],
// by default the current one, after asking: on the terminal if
// standard input is one, and else with rofi. It looks for the branch to
// check out: one named name, else those named name once each "/" is
// "-" or ending in "/<name>", which the user chooses between, else a new
// branch name from the HEAD of the main checkout. With -y it does not
// ask, and takes the first branch found.
//
// Remove removes worktree name, by default the worktree of the current
// workspace, and the directory stored for its workspace prefix; the
// branch is kept. It asks first, where init asks, unless -y is given.
// If the worktree has changes, agreeing discards them; with -y, git
// refuses to remove it unless -force is given.
//
// Pick lets the user choose a workspace with rofi and prints its name.
// It lists the workspaces other than the current one, grouped by
// project, in -columns columns of -lines rows. Ctrl+Enter takes the
// name as typed instead, for a new workspace. Ctrl+t shows or hides the
// worktrees of their projects that have no workspace yet; -worktrees
// shows them from the start. Listing them takes a moment, so they are
// hidden by default.
// It exits with status 1 if the user cancels. With -go, it goes to the
// workspace instead of printing it, as ws go does; this way xmonad can
// start it with spawn, rather than wait for it while rofi runs and lock
// up input.
//
// Go asks xmonad to show a workspace, creating it if it is new, with a
// WS_GO client message that the xmonad configuration handles.
//
// Rofi runs rofi with the given arguments, its window centred on the
// xmonad screen that shows the current workspace and 75% as wide as it.
// rofi itself knows only monitors, not the screens xmonad splits them
// into. Arguments can override the placement: rofi takes the last of
// each option and theme property. A wrapper script named rofi, before
// the real one in $PATH, makes every rofi run so; ws rofi skips such a
// wrapper when it looks for rofi. The windows of ws pick and of the
// questions ws asks are placed the same way.
package main

import (
	"errors"
	"fmt"
	"log"
	"os"
)

// errFalse makes ws exit with status 1 without a message, as a
// test that fails does.
var errFalse = errors.New("false")

func main() {
	log.SetFlags(0)
	log.SetPrefix("ws: ")
	if len(os.Args) < 2 {
		usage()
	}
	var err error
	switch cmd, args := os.Args[1], os.Args[2:]; cmd {
	case "name":
		err = runName(args)
	case "dir":
		err = runDir(args)
	case "action":
		err = runAction(args)
	case "wt":
		err = runWt(args)
	case "pick":
		err = runPick(args)
	case "rofi":
		err = runRofi(args)
	case "go":
		err = runGo(args)
	default:
		usage()
	}
	if errors.Is(err, errFalse) {
		os.Exit(1)
	}
	if err != nil {
		log.Fatal(err)
	}
}

func usage() {
	fmt.Fprintln(os.Stderr, `usage: ws name [-p | -s]
       ws dir [-set | -unset] [-pick] [workspace]
       ws action [workspace]
       ws wt list [-l] [-pick] [workspace]
       ws wt add [-branch branch] [-pick] [project@]name
       ws wt init [-y] [-pick] [workspace]
       ws wt remove [-force] [-y] [-pick] [[project@]name]
       ws pick [-go] [-p prompt] [-columns n] [-lines n] [-worktrees]
       ws go workspace
       ws rofi [rofi arguments]`)
	os.Exit(2)
}

// workspaceArg returns the workspace named by args, or the current
// workspace if args is empty.
func workspaceArg(args []string) (workspace, error) {
	switch len(args) {
	case 0:
		return currentWorkspace()
	case 1:
		return workspace(args[0]), nil
	}
	usage()
	panic("unreachable")
}

// output prints s, followed by a newline only on a terminal: callers
// such as Emacs's shell-command-to-string use the output as it is.
func output(s string) {
	fmt.Print(s)
	if isTerminal(os.Stdout) {
		fmt.Println()
	}
}
