//go:build linux

package main

import (
	"cmp"
	"errors"
	"log"
	"os"
	"os/exec"
	"syscall"
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

The launchers find the workspace in the environment variables WS_NAME,
WS_PREFIX, WS_PROJECT, WS_WORKTREE, WS_SUFFIX and WS_DIR.
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
func action(w workspace) error {
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
	for k, v := range map[string]string{
		"WS_NAME":     string(w),
		"WS_PREFIX":   w.prefix(),
		"WS_PROJECT":  w.project(),
		"WS_WORKTREE": w.worktree(),
		"WS_SUFFIX":   suffix,
		"WS_DIR":      dir,
	} {
		os.Setenv(k, v)
	}

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

// execLauncher replaces ws with the program name from $PATH.
func execLauncher(name string, args ...string) error {
	path, err := exec.LookPath(name)
	if err != nil {
		return err
	}
	return syscall.Exec(path, append([]string{name}, args...), os.Environ())
}
