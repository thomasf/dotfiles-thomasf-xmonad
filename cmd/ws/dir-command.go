//go:build linux

package main

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// errNoDir reports that a workspace has no directory.
var errNoDir = errors.New("no directory")

func runDir(args []string) error {
	fs := newFlagSet("dir", "[-set | -unset] [-pick] [workspace]", `
Dir prints the directory a workspace works in, by default the current
one. It is the directory stored for the prefix with -set, if there is
one. Else it is the directory stored for the project, or the ghq
repository named like the project. For a worktree, the directory is
the sibling of the project's main checkout named <repo>@<worktree>, at
the same place within it.

-set stores the working directory for the prefix, in ~/.config/wsdirs,
and -unset removes it.
`)
	set := fs.Bool("set", false, "store the working directory for the workspace prefix")
	unset := fs.Bool("unset", false, "remove the directory stored for the workspace prefix")
	pick := fs.Bool("pick", false, "choose with rofi between repositories with the same name")
	fs.Parse(args)
	w, err := workspaceArg(fs)
	if err != nil {
		return err
	}
	if *set && *unset {
		return errors.New("cannot specify both -set and -unset")
	}
	switch {
	case *set:
		wd, err := os.Getwd()
		if err != nil {
			return err
		}
		return w.setStoredDir(wd)
	case *unset:
		return w.setStoredDir("")
	}
	dir, err := workspaceDir(w, resolvePicker(*pick))
	if err != nil {
		return err
	}
	output(dir)
	return nil
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
	if dir, err := w.storedDir(); dir != "" || err != nil {
		return dir, err
	}
	dir, err := projectDir(w.project(), pick)
	if err != nil || w.worktree() == "" {
		return dir, err
	}
	return worktreeDir(dir, w.worktree())
}

// projectDir returns the directory stored for project, or else the
// ghq repository named project.
func projectDir(project string, pick Picker) (string, error) {
	if dir, err := storedDir(project); dir != "" || err != nil {
		return dir, err
	}
	dirs, err := ghqFind(project)
	if err != nil {
		return "", err
	}
	switch len(dirs) {
	case 0:
		return "", fmt.Errorf("%w for %s", errNoDir, project)
	case 1:
		return dirs[0], nil
	default:
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
		return "", err
	}
	wt := filepath.Join(main.worktreePath(name), rel)
	if !fileExists(wt) {
		return "", fmt.Errorf("%w for worktree %s: %s does not exist", errNoDir, name, wt)
	}
	return wt, nil
}
