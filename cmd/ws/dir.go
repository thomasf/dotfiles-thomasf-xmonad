//go:build linux

package main

import (
	"bytes"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
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
	switch {
	case *set:
		wd, err := os.Getwd()
		if err != nil {
			return err
		}
		return storeDir(w.prefix(), wd)
	case *unset:
		return storeDir(w.prefix(), "")
	}
	p := noPick
	if *pick {
		p = rofiPick
	}
	dir, err := workspaceDir(w, p)
	if err != nil {
		return err
	}
	output(dir)
	return nil
}

// A picker chooses one of the directories of the repositories named
// name.
type picker func(name string, dirs []string) (string, error)

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
func workspaceDir(w workspace, pick picker) (string, error) {
	if dir, err := storedDir(w.prefix()); dir != "" || err != nil {
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
func projectDir(project string, pick picker) (string, error) {
	if dir, err := storedDir(project); dir != "" || err != nil {
		return dir, err
	}
	out, err := exec.Command("ghq", "list", "-e", "-p", project).Output()
	if err != nil {
		return "", fmt.Errorf("ghq list: %w", err)
	}
	switch dirs := strings.Fields(string(out)); len(dirs) {
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
	wt := filepath.Join(worktreePath(main, name), rel)
	if _, err := os.Stat(wt); errors.Is(err, fs.ErrNotExist) {
		return "", fmt.Errorf("%w for worktree %s: %s does not exist", errNoDir, name, wt)
	} else if err != nil {
		return "", err
	}
	return wt, nil
}

// mainCheckout returns the main checkout of the repository that dir
// is in, and the path of dir relative to the checkout it is in, which
// may be a worktree.
func mainCheckout(dir string) (main, rel string, err error) {
	dir, err = filepath.EvalSymlinks(dir)
	if err != nil {
		return "", "", err
	}
	top, err := git(dir, "rev-parse", "--show-toplevel")
	if err != nil {
		return "", "", err
	}
	common, err := git(dir, "rev-parse", "--path-format=absolute", "--git-common-dir")
	if err != nil {
		return "", "", err
	}
	if filepath.Base(common) != ".git" {
		return "", "", fmt.Errorf("%s: no main checkout", dir)
	}
	rel, err = filepath.Rel(top, dir)
	return filepath.Dir(common), rel, err
}

// worktreePath returns the directory of worktree name of the main
// checkout main.
func worktreePath(main, name string) string {
	return filepath.Join(filepath.Dir(main), filepath.Base(main)+"@"+name)
}

func git(dir string, args ...string) (string, error) {
	out, err := exec.Command("git", append([]string{"-C", dir}, args...)...).Output()
	if ee, ok := errors.AsType[*exec.ExitError](err); ok && len(ee.Stderr) > 0 {
		return "", fmt.Errorf("git %s in %s: %s", strings.Join(args, " "), dir, bytes.TrimSpace(ee.Stderr))
	}
	if err != nil {
		return "", fmt.Errorf("git %s in %s: %w", strings.Join(args, " "), dir, err)
	}
	return strings.TrimSpace(string(out)), nil
}

// wsdirsFile returns the file that stores the directory for prefix,
// shared with the wsdir-* functions in the bash configuration.
func wsdirsFile(prefix string) (string, error) {
	return configFile("wsdirs", prefix)
}

// configFile returns the file name in the directory dir of
// ~/.config.
func configFile(dir, name string) (string, error) {
	if name == "" || name == "." || name == ".." || strings.ContainsRune(name, '/') {
		return "", fmt.Errorf("bad file name %q in ~/.config/%s", name, dir)
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", err
	}
	return filepath.Join(home, ".config", dir, name), nil
}

// storedDir returns the directory stored for prefix, or "" if there is
// none.
func storedDir(prefix string) (string, error) {
	file, err := wsdirsFile(prefix)
	if err != nil {
		return "", err
	}
	b, err := os.ReadFile(file)
	if errors.Is(err, fs.ErrNotExist) {
		return "", nil
	}
	return strings.TrimSpace(string(b)), err
}

// storeDir stores dir for prefix, or removes the stored directory if
// dir is "".
func storeDir(prefix, dir string) error {
	file, err := wsdirsFile(prefix)
	if err != nil {
		return err
	}
	if dir == "" {
		if err := os.Remove(file); err != nil && !errors.Is(err, fs.ErrNotExist) {
			return err
		}
		return nil
	}
	if err := os.MkdirAll(filepath.Dir(file), 0o755); err != nil {
		return err
	}
	return os.WriteFile(file, []byte(dir), 0o644)
}
