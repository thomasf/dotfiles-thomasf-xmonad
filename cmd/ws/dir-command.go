//go:build linux

package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/peterbourgon/ff/v3/ffcli"
)

// errNoDir reports that a workspace has no directory.
var errNoDir = errors.New("no directory")

func newDirCommand() *ffcli.Command {
	fs := flag.NewFlagSet("ws dir", flag.ContinueOnError)
	set := fs.Bool("set", false, "store the working directory for the workspace prefix")
	unset := fs.Bool("unset", false, "remove the directory stored for the workspace prefix")
	pick := fs.Bool("pick", false, "choose with rofi between repositories with the same name")
	cmd := &ffcli.Command{
		Name:       "dir",
		ShortUsage: "ws dir [-set | -unset] [-pick] [workspace]",
		ShortHelp:  "print the directory a workspace works in",
		LongHelp: `Dir prints the directory a workspace works in, by default the current
one. It is the directory stored for the prefix with -set, if there is
one. Else it is the directory stored for the project, or the ghq
repository named like the project. For a worktree, the directory is
the sibling of the project's main checkout named <repo>@<worktree>, at
the same place within it.

-set stores the working directory for the prefix, in ~/.config/wsdirs,
and -unset removes it.`,
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
		vlog("dir: resolving directory for workspace %q (set=%v, unset=%v, pick=%v)", w, *set, *unset, *pick)
		if *set && *unset {
			return errors.New("cannot specify both -set and -unset")
		}
		switch {
		case *set:
			wd, err := os.Getwd()
			if err != nil {
				vlog("dir: failed to get working directory: %v", err)
				return err
			}
			vlog("dir: storing working directory %q for prefix %q", wd, w.prefix())
			return w.setStoredDir(wd)
		case *unset:
			vlog("dir: removing stored directory for prefix %q", w.prefix())
			return w.setStoredDir("")
		}
		dir, err := workspaceDir(w, resolvePicker(*pick))
		if err != nil {
			vlog("dir: failed to resolve directory for workspace %q: %v", w, err)
			return err
		}
		vlog("dir: workspace %q directory is %q", w, dir)
		output(dir)
		return nil
	}
	return cmd
}

func runDir(args []string) error {
	return runFFCLI(newDirCommand(), args)
}

// resolvePicker returns rofiPick if pick is true, or noPick if false.
func resolvePicker(pick bool) Picker {
	if pick {
		return rofiPick
	}
	return noPick
}

// A Picker chooses one of the directories of the repositories named
// name.
type Picker func(name string, dirs []string) (string, error)

func noPick(name string, dirs []string) (string, error) {
	return "", fmt.Errorf("%d repositories named %s: %s", len(dirs), name, strings.Join(dirs, " "))
}

func rofiPick(name string, dirs []string) (string, error) {
	cmd, err := rofiCommand(60, 0, "", "-dmenu", "-p", name)
	if err != nil {
		return "", err
	}
	cmd.Stdin = strings.NewReader(strings.Join(dirs, "\n"))
	out, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("rofi: %w", err)
	}
	dir := strings.TrimSpace(string(out))
	if dir == "" {
		return "", fmt.Errorf("%w for %s: none chosen", errNoDir, name)
	}
	return dir, nil
}

// workspaceDir returns the directory w works in. The error wraps
// errNoDir if w has none.
func workspaceDir(w Workspace, pick Picker) (string, error) {
	vlog("workspaceDir: resolving directory for workspace %q", w)
	if dir, err := w.storedDir(); dir != "" || err != nil {
		if err != nil {
			vlog("workspaceDir: storedDir error for %q: %v", w.prefix(), err)
		} else {
			vlog("workspaceDir: found stored directory for prefix %q: %q", w.prefix(), dir)
		}
		return dir, err
	}
	vlog("workspaceDir: no stored directory for prefix %q; checking project directory for %q", w.prefix(), w.project())
	dir, err := projectDir(w.project(), pick)
	if err != nil || w.worktree() == "" {
		if err != nil {
			vlog("workspaceDir: projectDir error for %q: %v", w.project(), err)
		} else {
			vlog("workspaceDir: project directory for %q is %q", w.project(), dir)
		}
		return dir, err
	}
	wt, err := worktreeDir(dir, w.worktree())
	if err != nil {
		vlog("workspaceDir: worktreeDir error for %q worktree %q: %v", dir, w.worktree(), err)
	} else {
		vlog("workspaceDir: resolved worktree directory for %q worktree %q: %q", dir, w.worktree(), wt)
	}
	return wt, err
}

// projectDir returns the directory stored for project, or else the
// ghq repository named project.
func projectDir(project string, pick Picker) (string, error) {
	if dir, err := storedDir(project); dir != "" || err != nil {
		if err != nil {
			vlog("projectDir: storedDir error for project %q: %v", project, err)
		} else {
			vlog("projectDir: found stored directory for project %q: %q", project, dir)
		}
		return dir, err
	}
	dirs, err := ghqFind(project)
	if err != nil {
		vlog("projectDir: ghqFind error for project %q: %v", project, err)
		return "", err
	}
	switch len(dirs) {
	case 0:
		vlog("projectDir: no ghq repository found for project %q", project)
		return "", fmt.Errorf("%w for %s", errNoDir, project)
	case 1:
		vlog("projectDir: found 1 repository for project %q: %q", project, dirs[0])
		return dirs[0], nil
	default:
		vlog("projectDir: found %d repositories for project %q; using picker", len(dirs), project)
		return pick(project, dirs)
	}
}

// worktreeDir returns the directory in worktree name that corresponds
// to dir: the main checkout of the repository that dir is in has a
// sibling named <repo>@<name>, and the result is at the same place
// within it.
func worktreeDir(dir, name string) (string, error) {
	main, rel, err := mainCheckout(dir)
	if err != nil {
		vlog("worktreeDir: mainCheckout error for %q: %v", dir, err)
		return "", err
	}
	wt := filepath.Join(main.worktreePath(name), rel)
	if !fileExists(wt) {
		vlog("worktreeDir: worktree directory %q does not exist", wt)
		return "", fmt.Errorf("%w for worktree %s: %s does not exist", errNoDir, name, wt)
	}
	return wt, nil
}
