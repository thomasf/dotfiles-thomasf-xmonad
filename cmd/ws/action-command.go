//go:build linux

package main

import (
	"cmp"
	"context"
	"errors"
	"flag"
	"log"
	"maps"
	"os"
	"os/exec"

	"github.com/peterbourgon/ff/v3/ffcli"
)

func newActionCommand() *ffcli.Command {
	fs := flag.NewFlagSet("ws action", flag.ContinueOnError)
	cmd := &ffcli.Command{
		Name:       "action",
		ShortUsage: "ws action [workspace]",
		ShortHelp:  "start what an empty workspace needs",
		LongHelp: `Action is run by xmonad when it opens an empty workspace, by default
the current one. For a workspace in a worktree that does not exist
yet, it first offers to create it, as wt init does. In the directory of
the workspace, if it has one, it runs the launcher w.<project> from
$PATH. If there is none, or it fails, it starts what the suffix calls
for:

    www, doc                     www-window
    term, terminal, bot, test    term, after asking whether to run pathenv
    emacs                        e
    files                 nautilus, in the workspace directory`,
		FlagSet:   fs,
		UsageFunc: commandUsage,
	}
	cmd.Exec = func(ctx context.Context, args []string) error {
		if len(args) > 1 {
			usageError(fs)
		}
		var w Workspace
		var err error
		if len(args) == 1 {
			w = Workspace(args[0])
		} else {
			w, err = currentWorkspace()
			if err != nil {
				return err
			}
		}
		return action(w)
	}
	return cmd
}

func runAction(args []string) error {
	return runFFCLI(newActionCommand(), args)
}

// action starts what workspace w needs when xmonad opens it empty. For
// a workspace in a worktree that does not exist, it first offers to
// create the worktree. On success, it does not return if it starts a
// launcher for the suffix.
func action(w Workspace) error {
	vlog("action: starting action for workspace %q", w)
	dir, err := workspaceDir(w, rofiPick)
	if errors.Is(err, errNoDir) && w.worktree() != "" {
		// A new worktree workspace: offer to create the worktree.
		vlog("action: worktree directory does not exist for %q; offering to initialize worktree", w)
		err = initWorktree(w, false, true)
		if err == nil {
			dir, err = workspaceDir(w, rofiPick)
		} else if errors.Is(err, errCancelled) {
			vlog("action: worktree initialization cancelled by user")
			err = errNoDir
		}
	}
	switch {
	case err == nil:
		vlog("action: changing directory to %q", dir)
		if err := os.Chdir(dir); err != nil {
			vlog("action: failed to chdir to %q: %v", dir, err)
			return err
		}
	case errors.Is(err, errNoDir):
		vlog("action: workspace %q has no directory; remaining in current directory", w)
	default:
		// A workspace whose directory cannot be found still gets its
		// launchers, started where ws was.
		vlog("action: error resolving directory for workspace %q: %v", w, err)
		log.Print(err)
	}
	suffix, _ := w.suffix()

	launcher := "w." + w.project()
	if path, err := exec.LookPath(launcher); err == nil {
		vlog("action: running preferred project launcher %q (%s)", launcher, path)
		cmd := exec.Command(path)
		cmd.Stdout, cmd.Stderr = os.Stdout, os.Stderr
		if err := cmd.Run(); err == nil {
			vlog("action: project launcher %q completed successfully", launcher)
			return nil
		} else {
			vlog("action: project launcher %q failed: %v; falling back to suffix launcher", launcher, err)
		}
	} else {
		vlog("action: preferred project launcher %q not found in $PATH: %v", launcher, err)
	}

	switch suffix {
	case "www", "doc":
		vlog("action: starting suffix launcher %q for suffix %q", "www-window", suffix)
		return execLauncher([]string{"www-window"}, nil)
	case "term", "terminal", "bot", "test":
		vlog("action: starting suffix launcher %q for suffix %q", "term", suffix)
		return termWithPathenvQuestion([]string{"term"}, nil)
	case "emacs":
		vlog("action: starting suffix launcher %q for suffix %q", "e", suffix)
		return execLauncher([]string{"e"}, nil)
	case "files":
		home, _ := os.UserHomeDir()
		target := cmp.Or(dir, home)
		vlog("action: starting suffix launcher %q for suffix %q in %q", "nautilus", suffix, target)
		return execLauncher([]string{"nautilus", "--new-window", "--no-default-window", "--no-desktop", target}, nil)
	default:
		vlog("action: no suffix launcher configured for suffix %q", suffix)
	}
	return nil
}

// termWithPathenvQuestion asks whether the terminal of a workspace should run
// pathenv, and starts it. It starts nothing if the user cancels.
func termWithPathenvQuestion(args []string, env map[string]string) error {
	msg := "Terminal"
	if wd, err := os.Getwd(); err == nil {
		msg += " in " + shortenHome(wd)
	}
	i, err := choose(msg, []string{"with pathenv", "without pathenv"})
	if errors.Is(err, errCancelled) {
		return nil
	}
	if len(args) == 0 {
		args = []string{"term"}
	}
	env = maps.Clone(env)
	if env == nil {
		env = make(map[string]string)
	}
	if i == 0 {
		env["PATHENV_SKIP"] = ""
	} else {
		env["PATHENV_SKIP"] = "1"
	}
	return execLauncher(args, env)
}
