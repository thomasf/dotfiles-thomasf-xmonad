//go:build linux

package main

import (
	"flag"
	"fmt"
	"strings"

	"github.com/BurntSushi/xgb"
	"github.com/BurntSushi/xgb/xproto"
)

// A workspace is an xmonad workspace name, <prefix>[.<suffix>], whose
// prefix is <project>[@<worktree>].
type workspace string

func (w workspace) prefix() string {
	p, _, _ := strings.Cut(string(w), ".")
	return p
}

// suffix returns the part of w after the first "." and reports
// whether w has one.
func (w workspace) suffix() (string, bool) {
	_, s, ok := strings.Cut(string(w), ".")
	return s, ok
}

func (w workspace) project() string {
	p, _, _ := strings.Cut(w.prefix(), "@")
	return p
}

// worktree returns the part of the prefix of w after the first "@",
// or "" if w is not in a worktree.
func (w workspace) worktree() string {
	_, t, _ := strings.Cut(w.prefix(), "@")
	return t
}

func runName(args []string) error {
	fs := flag.NewFlagSet("name", flag.ExitOnError)
	prefix := fs.Bool("p", false, "print the prefix")
	suffix := fs.Bool("s", false, "print the suffix, or exit with status 1 if there is none")
	fs.Parse(args)
	if fs.NArg() > 0 {
		usage()
	}
	w, err := currentWorkspace()
	if err != nil {
		return err
	}
	s := string(w)
	switch {
	case *prefix:
		s = w.prefix()
	case *suffix:
		var ok bool
		if s, ok = w.suffix(); !ok {
			return errFalse
		}
	}
	output(s)
	return nil
}

// currentWorkspace returns the current workspace. A "/" in its name is
// replaced by "_", as wsname did, so that the name can be used as a
// file name.
func currentWorkspace() (workspace, error) {
	names, cur, err := desktops()
	if err != nil {
		return "", err
	}
	if cur >= len(names) || names[cur] == "" {
		return "", fmt.Errorf("no name for desktop %d", cur)
	}
	return workspace(strings.ReplaceAll(names[cur], "/", "_")), nil
}

// desktops returns the names of all workspaces and the index of the
// current one, read from the EWMH properties xmonad sets on the root
// window.
func desktops() (names []string, cur int, err error) {
	X, err := xgb.NewConn()
	if err != nil {
		return nil, 0, err
	}
	defer X.Close()
	root := xproto.Setup(X).DefaultScreen(X).Root

	c, err := rootProperty(X, root, "_NET_CURRENT_DESKTOP")
	if err != nil {
		return nil, 0, err
	}
	if c.Format != 32 || len(c.Value) < 4 {
		return nil, 0, fmt.Errorf("no current desktop")
	}
	n, err := rootProperty(X, root, "_NET_DESKTOP_NAMES")
	if err != nil {
		return nil, 0, err
	}
	names = strings.Split(strings.TrimSuffix(string(n.Value), "\x00"), "\x00")
	return names, int(xgb.Get32(c.Value)), nil
}

func rootProperty(X *xgb.Conn, root xproto.Window, name string) (*xproto.GetPropertyReply, error) {
	atom, err := xproto.InternAtom(X, true, uint16(len(name)), name).Reply()
	if err != nil {
		return nil, err
	}
	if atom.Atom == xproto.AtomNone {
		return nil, fmt.Errorf("no %s property", name)
	}
	return xproto.GetProperty(X, false, root, atom.Atom, xproto.GetPropertyTypeAny, 0, 1<<16).Reply()
}
