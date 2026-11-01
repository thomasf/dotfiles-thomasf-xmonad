//go:build linux

package main

import (
	"errors"
	"flag"
	"fmt"
	"io/fs"
	"os"
	"strings"
)

// A Workspace is an xmonad workspace name, <prefix>[.<suffix>], whose
// prefix is <project>[@<worktree>].
type Workspace string

func (w Workspace) prefix() string {
	p, _, _ := strings.Cut(string(w), ".")
	return p
}

// suffix returns the part of w after the first "." and reports
// whether w has one.
func (w Workspace) suffix() (string, bool) {
	_, s, ok := strings.Cut(string(w), ".")
	return s, ok
}

func (w Workspace) project() string {
	p, _, _ := strings.Cut(w.prefix(), "@")
	return p
}

// worktree returns the part of the prefix of w after the first "@",
// or "" if w is not in a worktree.
func (w Workspace) worktree() string {
	_, t, _ := strings.Cut(w.prefix(), "@")
	return t
}

// wsdirsFile returns the file that stores the directory for prefix,
// shared with the wsdir-* functions in the bash configuration.
func wsdirsFile(prefix string) (string, error) {
	return configFile("wsdirs", prefix)
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
	return writeAtomic(file, []byte(dir), 0o644)
}

// storedDir returns the directory stored for w's prefix, or "" if there is none.
func (w Workspace) storedDir() (string, error) {
	return storedDir(w.prefix())
}

// setStoredDir stores dir for w's prefix, or removes it if dir is "".
func (w Workspace) setStoredDir(dir string) error {
	return storeDir(w.prefix(), dir)
}

// currentWorkspace returns the current workspace. A "/" in its name is
// replaced by "_", as wsname did, so that the name can be used as a
// file name.
func currentWorkspace() (Workspace, error) {
	names, cur, err := desktops()
	if err != nil {
		return "", err
	}
	if cur >= len(names) || names[cur] == "" {
		return "", fmt.Errorf("no name for desktop %d", cur)
	}
	return Workspace(strings.ReplaceAll(names[cur], "/", "_")), nil
}

// workspaceArg returns the workspace named by the arguments left in
// fs, or the current workspace if there are none.
func workspaceArg(fs *flag.FlagSet) (Workspace, error) {
	switch fs.NArg() {
	case 0:
		return currentWorkspace()
	case 1:
		return Workspace(fs.Arg(0)), nil
	}
	usageError(fs)
	panic("unreachable")
}
