//go:build linux

package main

import (
	"fmt"
	"strings"

	"github.com/jezek/xgb"
	"github.com/jezek/xgb/xproto"
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
	fs := newFlagSet("name", "[-p | -s]", `
Name prints the name of the current workspace. A workspace name is
<prefix>[.<suffix>], and the prefix is <project>[@<worktree>]. In
workspace hello@test.bot, the project is hello, the worktree test and
the suffix bot.

Output to a terminal ends in a newline; other output does not, so that
it can be used as it is.
`)
	prefix := fs.Bool("p", false, "print the prefix")
	suffix := fs.Bool("s", false, "print the suffix, or exit with status 1 if there is none")
	fs.Parse(args)
	if fs.NArg() > 0 {
		usageError(fs)
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

// occupiedDesktops returns which workspaces, by index, have windows on
// them: the _NET_WM_DESKTOP of each window in _NET_CLIENT_LIST.
func occupiedDesktops() (map[int]bool, error) {
	X, err := xgb.NewConn()
	if err != nil {
		return nil, err
	}
	defer X.Close()
	root := xproto.Setup(X).DefaultScreen(X).Root

	l, err := rootProperty(X, root, "_NET_CLIENT_LIST")
	if err != nil {
		return nil, err
	}
	desktop, err := internAtom(X, "_NET_WM_DESKTOP")
	if err != nil {
		return nil, err
	}
	// Ask for all windows at once, rather than waiting for each.
	var cookies []xproto.GetPropertyCookie
	for v := l.Value; len(v) >= 4; v = v[4:] {
		w := xproto.Window(xgb.Get32(v))
		cookies = append(cookies, xproto.GetProperty(X, false, w, desktop, xproto.AtomCardinal, 0, 1))
	}
	occupied := make(map[int]bool)
	for _, c := range cookies {
		// A window may be gone by now.
		if r, err := c.Reply(); err == nil && r.Format == 32 && len(r.Value) >= 4 {
			occupied[int(xgb.Get32(r.Value))] = true
		}
	}
	return occupied, nil
}
