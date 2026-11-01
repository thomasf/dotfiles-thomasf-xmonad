//go:build linux

package main

import (
	"cmp"
	"errors"
	"log"
	"os"
	"os/exec"
)

func runAction(args []string) error {
	fs := newFlagSet("action", "[workspace]", `
Action is run by xmonad when it opens an empty workspace, by default
the current one. For a workspace in a worktree that does not exist
yet, it first offers to create it, as wt init does. In the directory of
the workspace, if it has one, it runs the launcher w.<project> from
$PATH. If there is none, or it fails, it starts what the suffix calls
for:

    www, doc              www-window
    term, terminal, bot   term
    emacs                 e
    files                 nautilus, in the workspace directory
`)
	fs.Parse(args)
	w, err := workspaceArg(fs)
	if err != nil {
		return err
	}
	return action(w)
}

// action starts what workspace w needs when xmonad opens it empty. For
// a workspace in a worktree that does not exist, it first offers to
// create the worktree. On success, it does not return if it starts a
// launcher for the suffix.
func action(w Workspace) error {
	dir, err := workspaceDir(w, rofiPick)
	if errors.Is(err, errNoDir) && w.worktree() != "" {
		// A new worktree workspace: offer to create the worktree.
		err = initWorktree(w, false, true)
		if err == nil {
			dir, err = workspaceDir(w, rofiPick)
		} else if errors.Is(err, errCancelled) {
			err = errNoDir
		}
	}
	switch {
	case err == nil:
		if err := os.Chdir(dir); err != nil {
			return err
		}
	case errors.Is(err, errNoDir):
	default:
		// A workspace whose directory cannot be found still gets its
		// launchers, started where ws was.
		log.Print(err)
	}
	suffix, _ := w.suffix()

	if path, err := exec.LookPath("w." + w.project()); err == nil {
		cmd := exec.Command(path)
		cmd.Stdout, cmd.Stderr = os.Stdout, os.Stderr
		if cmd.Run() == nil {
			return nil
		}
	}

	switch suffix {
	case "www", "doc":
		return execLauncher("www-window")
	case "term", "terminal", "bot":
		return execLauncher("term")
	case "emacs":
		return execLauncher("e")
	case "files":
		home, _ := os.UserHomeDir()
		return execLauncher("nautilus", "--new-window", "--no-default-window", "--no-desktop", cmp.Or(dir, home))
	}
	return nil
}
