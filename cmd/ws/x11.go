//go:build linux

package main

import (
	"cmp"
	"fmt"
	"html"
	"os"
	"slices"
	"strings"

	"github.com/jezek/xgb"
	"github.com/jezek/xgb/randr"
	"github.com/jezek/xgb/xproto"
)

// X11Client manages an X11 connection, its root window, and interned atom cache.
type X11Client struct {
	conn  *xgb.Conn
	root  xproto.Window
	atoms map[string]xproto.Atom
}

// openX opens a connection to the X server and returns an X11Client.
func openX() (*X11Client, error) {
	conn, err := xgb.NewConn()
	if err != nil {
		return nil, err
	}
	return &X11Client{
		conn:  conn,
		root:  xproto.Setup(conn).DefaultScreen(conn).Root,
		atoms: make(map[string]xproto.Atom),
	}, nil
}

func (x *X11Client) close() {
	if x != nil && x.conn != nil {
		x.conn.Close()
	}
}

func (x *X11Client) internAtom(name string) (xproto.Atom, error) {
	if a, ok := x.atoms[name]; ok {
		return a, nil
	}
	a, err := xproto.InternAtom(x.conn, false, uint16(len(name)), name).Reply()
	if err != nil {
		return 0, err
	}
	x.atoms[name] = a.Atom
	return a.Atom, nil
}

func (x *X11Client) rootProperty(name string) (*xproto.GetPropertyReply, error) {
	atom, err := xproto.InternAtom(x.conn, true, uint16(len(name)), name).Reply()
	if err != nil {
		return nil, err
	}
	if atom.Atom == xproto.AtomNone {
		return nil, fmt.Errorf("no %s property", name)
	}
	return xproto.GetProperty(x.conn, false, x.root, atom.Atom, xproto.GetPropertyTypeAny, 0, 1<<16).Reply()
}

func (x *X11Client) rootPropertyUint32(name string) (uint32, error) {
	c, err := x.rootProperty(name)
	if err != nil {
		return 0, err
	}
	if c.Format != 32 || len(c.Value) < 4 {
		return 0, fmt.Errorf("no %s property", name)
	}
	return xgb.Get32(c.Value), nil
}

// parseWindows parses a byte slice of 32-bit window IDs.
func parseWindows(data []byte) []xproto.Window {
	wins := make([]xproto.Window, 0, len(data)/4)
	for len(data) >= 4 {
		wins = append(wins, xproto.Window(xgb.Get32(data)))
		data = data[4:]
	}
	return wins
}

// sendClientMessage sends a client message event of typ with data on the root
// window for target window w.
func (x *X11Client) sendClientMessage(w xproto.Window, typ string, data []uint32) error {
	t, err := x.internAtom(typ)
	if err != nil {
		return err
	}
	var d [5]uint32
	for i, v := range data {
		if i < 5 {
			d[i] = v
		}
	}
	ev := xproto.ClientMessageEvent{
		Format: 32,
		Window: w,
		Type:   t,
		Data:   xproto.ClientMessageDataUnionData32New(d[:]),
	}
	return xproto.SendEventChecked(x.conn, false, x.root,
		xproto.EventMaskSubstructureNotify|xproto.EventMaskSubstructureRedirect, string(ev.Bytes())).Check()
}

// sendXmonad sends xmonad a client message of type typ on the root
// window, with datum as its first datum.
func (x *X11Client) sendXmonad(typ string, datum uint32) error {
	return x.sendClientMessage(x.root, typ, []uint32{datum, xproto.TimeCurrentTime})
}

func (x *X11Client) sendWorkspaceMessage(typ, name string) error {
	a, err := x.internAtom(name)
	if err != nil {
		return err
	}
	return x.sendXmonad(typ, uint32(a))
}

func sendWorkspaceMessage(typ, name string) error {
	x, err := openX()
	if err != nil {
		return err
	}
	defer x.close()
	return x.sendWorkspaceMessage(typ, name)
}

// desktops returns the names of all workspaces and the index of the
// current one, read from the EWMH properties xmonad sets on the root
// window.
func (x *X11Client) desktops() (names []string, cur int, err error) {
	cur32, err := x.rootPropertyUint32("_NET_CURRENT_DESKTOP")
	if err != nil {
		return nil, 0, err
	}
	n, err := x.rootProperty("_NET_DESKTOP_NAMES")
	if err != nil {
		return nil, 0, err
	}
	names = strings.Split(strings.TrimSuffix(string(n.Value), "\x00"), "\x00")
	return names, int(cur32), nil
}

// occupiedDesktops returns which workspaces, by index, have windows on
// them: the _NET_WM_DESKTOP of each window in _NET_CLIENT_LIST.
func (x *X11Client) occupiedDesktops() (map[int]bool, error) {
	l, err := x.rootProperty("_NET_CLIENT_LIST")
	if err != nil {
		return nil, err
	}
	desktop, err := x.internAtom("_NET_WM_DESKTOP")
	if err != nil {
		return nil, err
	}
	// Ask for all windows at once, rather than waiting for each.
	var cookies []xproto.GetPropertyCookie
	for _, w := range parseWindows(l.Value) {
		cookies = append(cookies, xproto.GetProperty(x.conn, false, w, desktop, xproto.AtomCardinal, 0, 1))
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

// visibleDesktops returns which workspaces, by index, are shown on
// screens other than the current one, or nil if it cannot tell. xmonad
// sets the _NET_DESKTOP_VIEWPORT of a workspace to the corner of the
// screen showing it, and that of hidden ones to the current screen's.
func (x *X11Client) visibleDesktops() map[int]bool {
	cur32, err := x.rootPropertyUint32("_NET_CURRENT_DESKTOP")
	if err != nil {
		return nil
	}
	cur := int(cur32)
	vp, err := x.rootProperty("_NET_DESKTOP_VIEWPORT")
	if err != nil {
		return nil
	}
	origins := parseViewportOrigins(vp.Value)
	if cur >= len(origins) {
		return nil
	}
	visible := make(map[int]bool)
	for i, o := range origins {
		if o != origins[cur] {
			visible[i] = true
		}
	}
	return visible
}

func desktops() (names []string, cur int, err error) {
	x, err := openX()
	if err != nil {
		return nil, 0, err
	}
	defer x.close()
	return x.desktops()
}

// --- Screens and Monitors (Geometry and RandR) ---

// A Rect is an area of the root window.
type Rect struct{ x, y, w, h int }

func (r Rect) contains(p Point) bool {
	return p.x >= r.x && p.x < r.x+r.w && p.y >= r.y && p.y < r.y+r.h
}

func (r Rect) center() Point {
	return Point{r.x + r.w/2, r.y + r.h/2}
}

type Point struct{ x, y int }

// A Monitor is a physical monitor, named as xrandr names its output.
type Monitor struct {
	name string
	Rect
}

// currentScreen returns the area of the xmonad screen that shows the
// current workspace, and the monitor it is on.
func (x *X11Client) currentScreen() (Rect, Monitor, error) {
	cur32, err := x.rootPropertyUint32("_NET_CURRENT_DESKTOP")
	if err != nil {
		return Rect{}, Monitor{}, err
	}
	cur := int(cur32)
	vp, err := x.rootProperty("_NET_DESKTOP_VIEWPORT")
	if err != nil {
		return Rect{}, Monitor{}, err
	}
	origins := parseViewportOrigins(vp.Value)
	if cur >= len(origins) {
		return Rect{}, Monitor{}, fmt.Errorf("no viewport for desktop %d", cur)
	}
	mons, err := x.monitors()
	if err != nil {
		return Rect{}, Monitor{}, err
	}
	return screenAt(origins[cur], origins, mons)
}

func currentScreen() (Rect, Monitor, error) {
	x, err := openX()
	if err != nil {
		return Rect{}, Monitor{}, err
	}
	defer x.close()
	return x.currentScreen()
}

// screenAt returns the area of the xmonad screen whose top left corner
// is o, and its monitor. xmonad screens can be smaller than monitors,
// as when layoutScreens splits one, and xmonad publishes only their
// corners, in _NET_DESKTOP_VIEWPORT; origins holds those of all
// workspaces, among them every visible screen's. So a screen is taken to
// end where the next screen on its monitor starts, to the right or
// below, or else at the edge of the monitor.
func screenAt(o Point, origins []Point, mons []Monitor) (Rect, Monitor, error) {
	const near = 16 // pixels between corners on one row or column
	for _, m := range mons {
		if !m.contains(o) {
			continue
		}
		right, bottom := m.x+m.w, m.y+m.h
		for _, p := range origins {
			if !m.contains(p) {
				continue
			}
			if p.x > o.x && abs(p.y-o.y) < near {
				right = min(right, p.x)
			}
			if p.y > o.y && abs(p.x-o.x) < near {
				bottom = min(bottom, p.y)
			}
		}
		return Rect{o.x, o.y, right - o.x, bottom - o.y}, m, nil
	}
	return Rect{}, Monitor{}, fmt.Errorf("no monitor at %d,%d", o.x, o.y)
}

func abs(n int) int {
	return max(n, -n)
}

func parseViewportOrigins(data []byte) []Point {
	var origins []Point
	for i := 0; i+8 <= len(data); i += 8 {
		origins = append(origins, Point{int(xgb.Get32(data[i:])), int(xgb.Get32(data[i+4:]))})
	}
	return origins
}

// monitors returns the active monitors.
func (x *X11Client) monitors() ([]Monitor, error) {
	if err := randr.Init(x.conn); err != nil {
		return nil, err
	}
	res, err := randr.GetScreenResourcesCurrent(x.conn, x.root).Reply()
	if err != nil {
		return nil, err
	}
	var mons []Monitor
	for _, crtc := range res.Crtcs {
		info, err := randr.GetCrtcInfo(x.conn, crtc, res.ConfigTimestamp).Reply()
		if err != nil || info.Width == 0 || len(info.Outputs) == 0 {
			continue
		}
		out, err := randr.GetOutputInfo(x.conn, info.Outputs[0], res.ConfigTimestamp).Reply()
		if err != nil {
			continue
		}
		mons = append(mons, Monitor{string(out.Name), Rect{int(info.X), int(info.Y), int(info.Width), int(info.Height)}})
	}
	return mons, nil
}

// --- Windows and Actions (Client Management) ---

type WindowAction int

const (
	actionFocus WindowAction = iota
	actionBring
	actionClose
	actionToggleAll
)

// A Window is a top-level window xmonad manages.
type Window struct {
	id           xproto.Window
	title        string
	class        string
	workspace    string
	desktopIndex int
	pid          int
	cwd          string
	active       bool
	urgent       bool
	stackRank    int
	activeCmd    string
	searchTerms  []string
}

// isTerminal reports whether the window belongs to a terminal emulator or shell.
func (w Window) isTerminal() bool {
	lc := strings.ToLower(w.class)
	for t := range terminalComms {
		if strings.Contains(lc, t) {
			return true
		}
	}
	return commonShells[w.activeCmd] || w.activeCmd != ""
}

// focus asks xmonad to focus window w.
func (w Window) focus(x *X11Client) error {
	return x.sendXmonad("WS_FOCUS", uint32(w.id))
}

// bring moves window w to desktop and focuses it.
func (w Window) bring(x *X11Client, desktop int) error {
	if err := x.sendClientMessage(w.id, "_NET_WM_DESKTOP", []uint32{uint32(desktop), 2}); err != nil {
		return err
	}
	return w.focus(x)
}

// close asks the window manager to close window w.
func (w Window) close(x *X11Client) error {
	return x.sendClientMessage(w.id, "_NET_CLOSE_WINDOW", []uint32{uint32(xproto.TimeCurrentTime), 2})
}

// listWindows returns the windows in _NET_CLIENT_LIST, sorted in MRU order
// (with urgent windows first and active window last). names are the workspace names,
// by index, for _NET_WM_DESKTOP. tree provides process information.
func (x *X11Client) listWindows(names []string, tree *ProcTree) ([]Window, error) {
	l, err := x.rootProperty("_NET_CLIENT_LIST")
	if err != nil {
		return nil, err
	}

	activeWid := xproto.Window(0)
	if val, err := x.rootPropertyUint32("_NET_ACTIVE_WINDOW"); err == nil {
		activeWid = xproto.Window(val)
	}

	stackRanks := make(map[xproto.Window]int)
	if stackProp, err := x.rootProperty("_NET_CLIENT_LIST_STACKING"); err == nil {
		for _, wid := range parseWindows(stackProp.Value) {
			stackRanks[wid] = len(stackRanks)
		}
	}

	atoms := make(map[string]xproto.Atom)
	for _, name := range []string{
		"_NET_WM_DESKTOP",
		"_NET_WM_NAME",
		"UTF8_STRING",
		"_NET_WM_PID",
		"_NET_WM_STATE",
		"_NET_WM_STATE_DEMANDS_ATTENTION",
	} {
		if atoms[name], err = x.internAtom(name); err != nil {
			return nil, err
		}
	}
	// Ask for all windows at once, rather than waiting for each.
	type cookies struct {
		desktop xproto.GetPropertyCookie
		netName xproto.GetPropertyCookie
		name    xproto.GetPropertyCookie
		class   xproto.GetPropertyCookie
		pid     xproto.GetPropertyCookie
		state   xproto.GetPropertyCookie
		hints   xproto.GetPropertyCookie
	}
	ids := parseWindows(l.Value)
	var cs []cookies
	for _, w := range ids {
		cs = append(cs, cookies{
			desktop: xproto.GetProperty(x.conn, false, w, atoms["_NET_WM_DESKTOP"], xproto.AtomCardinal, 0, 1),
			netName: xproto.GetProperty(x.conn, false, w, atoms["_NET_WM_NAME"], atoms["UTF8_STRING"], 0, 1024),
			name:    xproto.GetProperty(x.conn, false, w, xproto.AtomWmName, xproto.GetPropertyTypeAny, 0, 1024),
			class:   xproto.GetProperty(x.conn, false, w, xproto.AtomWmClass, xproto.AtomString, 0, 256),
			pid:     xproto.GetProperty(x.conn, false, w, atoms["_NET_WM_PID"], xproto.AtomCardinal, 0, 1),
			state:   xproto.GetProperty(x.conn, false, w, atoms["_NET_WM_STATE"], xproto.GetPropertyTypeAny, 0, 32),
			hints:   xproto.GetProperty(x.conn, false, w, xproto.AtomWmHints, xproto.GetPropertyTypeAny, 0, 9),
		})
	}
	var wins []Window
	for i, c := range cs {
		// A window may be gone by now.
		d, err := c.desktop.Reply()
		if err != nil || d.Format != 32 || len(d.Value) < 4 {
			continue
		}
		w := Window{id: ids[i], desktopIndex: -1}
		if n := int(xgb.Get32(d.Value)); n < len(names) {
			w.desktopIndex = n
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
		if p, err := c.pid.Reply(); err == nil && p.Format == 32 && len(p.Value) >= 4 {
			w.pid = int(xgb.Get32(p.Value))
			if tree != nil && w.pid > 0 {
				details := tree.details(w.pid)
				w.searchTerms, w.activeCmd, w.cwd = details.searchTerms, details.activeCmd, details.cwd
			}
		}

		urgent := false
		if s, err := c.state.Reply(); err == nil && s.Format == 32 {
			for v := s.Value; len(v) >= 4; v = v[4:] {
				if xproto.Atom(xgb.Get32(v)) == atoms["_NET_WM_STATE_DEMANDS_ATTENTION"] {
					urgent = true
					break
				}
			}
		}
		if !urgent {
			if h, err := c.hints.Reply(); err == nil && h.Format == 32 && len(h.Value) >= 4 {
				flags := xgb.Get32(h.Value)
				if flags&(1<<8) != 0 {
					urgent = true
				}
			}
		}

		rank, ok := stackRanks[ids[i]]
		if !ok {
			rank = -1
		}
		w.active = (ids[i] == activeWid)
		w.urgent = urgent
		w.stackRank = rank

		wins = append(wins, w)
	}

	sortWindows(wins)
	return wins, nil
}

// sortWindows sorts wins so that:
// 1. Non-active windows come before the active window (putting the previous window at index 0).
// 2. Urgent windows come before non-urgent windows.
// 3. Higher MRU stack ranks come first (newest to oldest).
func sortWindows(wins []Window) {
	slices.SortFunc(wins, func(a, b Window) int {
		if a.active != b.active {
			if a.active {
				return 1
			}
			return -1
		}
		if a.urgent != b.urgent {
			if a.urgent {
				return -1
			}
			return 1
		}
		return cmp.Compare(b.stackRank, a.stackRank)
	})
}

// textProperty returns the text of a property such as WM_NAME, which is
// Latin-1 if its type is STRING and taken to be UTF-8 otherwise.
func textProperty(r *xproto.GetPropertyReply) string {
	if r.Type != xproto.AtomString {
		return strings.ToValidUTF8(string(r.Value), "")
	}
	rs := make([]rune, len(r.Value))
	for i, b := range r.Value {
		rs[i] = rune(b)
	}
	return string(rs)
}

// row formats window w as a single formatted rofi row:
// ! Title  — Class  Workspace  Path
func (w Window) row(onScreen map[int]bool, curDesktop int) string {
	accent := accentColor()
	visibleCol := visibleColor()
	hereCol := paletteColor("green")
	urgentBg := paletteColor("blue")
	urgentFg := paletteColor("bg_0")

	title := strings.ReplaceAll(w.title, "\n", " ")
	title = strings.TrimSpace(title)

	wsColor := accent
	if w.desktopIndex == curDesktop {
		wsColor = hereCol
	} else if w.desktopIndex >= 0 && onScreen[w.desktopIndex] {
		wsColor = visibleCol
	}
	wsBadge := pangoColor(w.workspace, wsColor)

	var pathStr string
	if w.cwd != "" {
		home, _ := os.UserHomeDir()
		if w.cwd != home || w.isTerminal() {
			pathStr = shortenHome(w.cwd)
		}
	}

	var lineParts []string
	lineParts = append(lineParts, html.EscapeString(title))
	if w.class != "" {
		lineParts = append(lineParts, pangoDim("— "+w.class, 60))
	}
	lineParts = append(lineParts, wsBadge)
	if pathStr != "" {
		lineParts = append(lineParts, pangoDim(pathStr, 70))
	}

	displayParts := strings.Join(lineParts, "  ")
	if w.urgent {
		displayParts = fmt.Sprintf("<span background='%s' foreground='%s'><b>!</b></span> %s", urgentBg, urgentFg, displayParts)
	}

	filterTerms := append([]string{w.title, w.class, w.workspace}, w.searchTerms...)
	if pathStr != "" {
		filterTerms = append(filterTerms, pathStr)
	}
	if w.desktopIndex == curDesktop {
		filterTerms = append(filterTerms, "here")
	}
	if w.urgent {
		filterTerms = append(filterTerms, "!", "urgent")
	}
	filterStr := strings.Join(filterTerms, " ")

	return fmt.Sprintf("%s\x00display\x1f%s", filterStr, displayParts)
}

// windowRows returns the rofi rows for wins on a single formatted line:
// ! Title  — Class  Workspace  Path
// All running process metadata is included in the filter string for searching.
func windowRows(wins []Window, onScreen map[int]bool, curDesktop int) []string {
	rows := make([]string, len(wins))
	for i, w := range wins {
		rows[i] = w.row(onScreen, curDesktop)
	}
	return rows
}
