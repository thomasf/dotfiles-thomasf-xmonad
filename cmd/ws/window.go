//go:build linux

package main

import (
	"fmt"
	"html"
	"strconv"
	"strings"

	"github.com/jezek/xgb"
	"github.com/jezek/xgb/xproto"
)

func runWindow(args []string) error {
	if len(args) > 0 {
		usage()
	}
	names, _, err := desktops()
	if err != nil {
		return err
	}
	X, err := xgb.NewConn()
	if err != nil {
		return err
	}
	defer X.Close()
	wins, err := listWindows(X, names)
	if err != nil {
		return err
	}
	w, err := rofiPickWindow(wins)
	if err != nil {
		return err
	}
	return sendXmonad(X, "WS_FOCUS", uint32(w))
}

// A window is a top-level window xmonad manages.
type window struct {
	id        xproto.Window
	title     string
	class     string
	workspace string
}

// listWindows returns the windows in _NET_CLIENT_LIST, in its order,
// which is xmonad's order of workspaces. names are the workspace names,
// by index, for _NET_WM_DESKTOP.
func listWindows(X *xgb.Conn, names []string) ([]window, error) {
	root := xproto.Setup(X).DefaultScreen(X).Root
	l, err := rootProperty(X, root, "_NET_CLIENT_LIST")
	if err != nil {
		return nil, err
	}
	atoms := make(map[string]xproto.Atom)
	for _, name := range []string{"_NET_WM_DESKTOP", "_NET_WM_NAME", "UTF8_STRING"} {
		if atoms[name], err = internAtom(X, name); err != nil {
			return nil, err
		}
	}
	// Ask for all windows at once, rather than waiting for each.
	type cookies struct{ desktop, netName, name, class xproto.GetPropertyCookie }
	var ids []xproto.Window
	var cs []cookies
	for v := l.Value; len(v) >= 4; v = v[4:] {
		w := xproto.Window(xgb.Get32(v))
		ids = append(ids, w)
		cs = append(cs, cookies{
			xproto.GetProperty(X, false, w, atoms["_NET_WM_DESKTOP"], xproto.AtomCardinal, 0, 1),
			xproto.GetProperty(X, false, w, atoms["_NET_WM_NAME"], atoms["UTF8_STRING"], 0, 1024),
			xproto.GetProperty(X, false, w, xproto.AtomWmName, xproto.GetPropertyTypeAny, 0, 1024),
			xproto.GetProperty(X, false, w, xproto.AtomWmClass, xproto.AtomString, 0, 256),
		})
	}
	var wins []window
	for i, c := range cs {
		// A window may be gone by now.
		d, err := c.desktop.Reply()
		if err != nil || d.Format != 32 || len(d.Value) < 4 {
			continue
		}
		w := window{id: ids[i]}
		if n := int(xgb.Get32(d.Value)); n < len(names) {
			w.workspace = names[n]
		}
		if r, err := c.netName.Reply(); err == nil && len(r.Value) > 0 {
			w.title = string(r.Value)
		} else if r, err := c.name.Reply(); err == nil {
			w.title = textProperty(r)
		}
		if r, err := c.class.Reply(); err == nil {
			// WM_CLASS is the instance and the class, each ending in
			// a NUL.
			if f := strings.Split(string(r.Value), "\x00"); len(f) >= 2 {
				w.class = f[1]
			}
		}
		wins = append(wins, w)
	}
	return wins, nil
}

// textProperty returns the text of a property such as WM_NAME, which is
// Latin-1 if its type is STRING and taken to be UTF-8 otherwise.
func textProperty(r *xproto.GetPropertyReply) string {
	if r.Type != xproto.AtomString {
		return strings.ToValidUTF8(string(r.Value), "�")
	}
	rs := make([]rune, len(r.Value))
	for i, b := range r.Value {
		rs[i] = rune(b)
	}
	return string(rs)
}

// rofiPickWindow lets the user pick one of wins with rofi and returns
// its id. It returns errFalse if the user cancels.
func rofiPickWindow(wins []window) (xproto.Window, error) {
	cmd, err := rofiCommand(rofiWidth, 0, "",
		"-dmenu", "-i", "-markup-rows", "-no-custom", "-format", "i", "-p", "window")
	if err != nil {
		return 0, err
	}
	cmd.Stdin = strings.NewReader(strings.Join(windowRows(wins), "\n"))
	out, err := cmd.Output()
	if err != nil {
		return 0, errFalse
	}
	n, err := strconv.Atoi(strings.TrimSpace(string(out)))
	if err != nil || n < 0 || n >= len(wins) {
		return 0, fmt.Errorf("rofi: unexpected output %q", out)
	}
	return wins[n].id, nil
}

// windowRows returns the rofi rows for wins: the title, then the class
// and the workspace, as the window search in xmonad showed them.
func windowRows(wins []window) []string {
	rows := make([]string, len(wins))
	for i, w := range wins {
		// A newline would make two rows of one.
		title := strings.ReplaceAll(w.title, "\n", " ")
		rows[i] = fmt.Sprintf("%s<span font_family='Consolas'> — %s</span> ❨%s❩",
			html.EscapeString(title), html.EscapeString(w.class), html.EscapeString(w.workspace))
	}
	return rows
}
