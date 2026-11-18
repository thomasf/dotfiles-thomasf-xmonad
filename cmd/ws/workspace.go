//go:build linux

package main

import (
	"errors"
	"flag"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"unicode"
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

// fileName returns w as a file name; see [fileName].
func (w Workspace) fileName() string {
	return fileName(string(w))
}

// fileName returns s, a workspace name or a part of one, in the form used
// wherever a workspace is named outside xmonad: in file and socket names and
// as the remote control name of critty. Letters, digits and "._@+-" are kept
// and every other character becomes "_", as does a leading "-".
func fileName(s string) string {
	s = strings.Map(func(r rune) rune {
		if unicode.IsLetter(r) || unicode.IsDigit(r) || strings.ContainsRune("._@+-", r) {
			return r
		}
		return '_'
	}, s)
	if rest, ok := strings.CutPrefix(s, "-"); ok {
		s = "_" + rest
	}
	return s
}

// StoredDir associates a workspace prefix with its stored directory.
type StoredDir struct {
	Prefix string
	Dir    string
}

// wsdirsDir returns the path to ~/.config/wsdirs.
func wsdirsDir() (string, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return "", err
	}
	return filepath.Join(home, ".config", "wsdirs"), nil
}

// storedDirs returns all stored workspace prefixes and their directories,
// sorted by prefix.
func storedDirs() ([]StoredDir, error) {
	dir, err := wsdirsDir()
	if err != nil {
		return nil, err
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return nil, nil
		}
		return nil, err
	}
	var res []StoredDir
	for _, e := range entries {
		if e.IsDir() || strings.HasPrefix(e.Name(), ".") {
			continue
		}
		content, err := os.ReadFile(filepath.Join(dir, e.Name()))
		if err != nil {
			continue
		}
		target := strings.TrimSpace(string(content))
		if target != "" {
			res = append(res, StoredDir{
				Prefix: e.Name(),
				Dir:    target,
			})
		}
	}
	slices.SortFunc(res, func(a, b StoredDir) int {
		return strings.Compare(a.Prefix, b.Prefix)
	})
	return res, nil
}

// wsdirsFile returns the file that stores the directory for prefix,
// named by [fileName].
func wsdirsFile(prefix string) (string, error) {
	return configFile("wsdirs", fileName(prefix))
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

// currentWorkspace returns the current workspace.
func currentWorkspace() (Workspace, error) {
	names, cur, err := desktops()
	if err != nil {
		vlog("currentWorkspace: desktops query failed: %v", err)
		return "", err
	}
	if cur >= len(names) || names[cur] == "" {
		vlog("currentWorkspace: no name for desktop %d (total desktops: %d)", cur, len(names))
		return "", fmt.Errorf("no name for desktop %d", cur)
	}
	vlog("currentWorkspace: active desktop is %d (%s)", cur, names[cur])
	return Workspace(names[cur]), nil
}

// contextWorkspace returns the workspace that a command run in a terminal
// belongs to: $WS_WORKSPACE, which ws term sets for the terminals it starts,
// or else the current workspace. An agent in a terminal on a workspace
// that is not shown thereby still addresses its own workspace.
func contextWorkspace() (Workspace, error) {
	if w := os.Getenv("WS_WORKSPACE"); w != "" {
		vlog("contextWorkspace: resolved from $WS_WORKSPACE: %q", w)
		return Workspace(w), nil
	}
	vlog("contextWorkspace: $WS_WORKSPACE not set; querying current workspace")
	return currentWorkspace()
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
