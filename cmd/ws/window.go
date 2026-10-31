//go:build linux

package main

import (
	"cmp"
	"errors"
	"fmt"
	"html"
	"os"
	"os/exec"
	"slices"
	"strconv"
	"strings"

	"github.com/jezek/xgb"
	"github.com/jezek/xgb/xproto"
)

func runWindow(args []string) error {
	fs := newFlagSet("window", "[-all] [-width pct] [-height pct]", `
Window lets the user choose a window with rofi, listed with its class,
workspace, and running processes, and asks xmonad to focus it, switching
to its workspace, with a WS_FOCUS client message that the xmonad
configuration handles.

    Enter    focus the window and switch to its workspace
    Alt+b    bring the window to the current workspace
    Alt+k    close the window
    Alt+a    toggle showing all windows (including dash and NSP)

With -all, it starts by showing all windows.
`)
	withAll := fs.Bool("all", false, "include all windows (including dash and NSP)")
	width := fs.Int("width", 95, "rofi `width` percent")
	height := fs.Int("height", 85, "rofi `height` percent")
	fs.Parse(args)
	if fs.NArg() > 0 {
		usageError(fs)
	}
	names, cur, err := desktops()
	if err != nil {
		return err
	}
	X, err := xgb.NewConn()
	if err != nil {
		return err
	}
	defer X.Close()

	tree := readProcTree()
	wins, err := listWindows(X, names, tree)
	if err != nil {
		return err
	}
	if len(wins) == 0 {
		return nil
	}

	onScreen := visibleDesktops()
	showAll := *withAll
	for {
		var displayed []window
		for _, w := range wins {
			if !showAll && (w.workspace == "NSP" || w.workspace == "dash") {
				continue
			}
			displayed = append(displayed, w)
		}
		if len(displayed) == 0 {
			if !showAll {
				showAll = true
				continue
			}
			return nil
		}
		w, action, err := rofiPickWindow(displayed, onScreen, cur, showAll, *width, *height)
		if err != nil {
			return err
		}
		switch action {
		case actionToggleAll:
			showAll = !showAll
			continue
		case actionFocus:
			return sendXmonad(X, "WS_FOCUS", uint32(w.id))
		case actionBring:
			if err := sendClientMessage(X, w.id, "_NET_WM_DESKTOP", []uint32{uint32(cur), 2}); err != nil {
				return err
			}
			return sendXmonad(X, "WS_FOCUS", uint32(w.id))
		case actionClose:
			return sendClientMessage(X, w.id, "_NET_CLOSE_WINDOW", []uint32{uint32(xproto.TimeCurrentTime), 2})
		}
		return nil
	}
}

type windowAction int

const (
	actionFocus windowAction = iota
	actionBring
	actionClose
	actionToggleAll
)

// A window is a top-level window xmonad manages.
type window struct {
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

// listWindows returns the windows in _NET_CLIENT_LIST, sorted in MRU order
// (with urgent windows first and active window last). names are the workspace names,
// by index, for _NET_WM_DESKTOP. tree provides process information.
func listWindows(X *xgb.Conn, names []string, tree *procTree) ([]window, error) {
	root := xproto.Setup(X).DefaultScreen(X).Root
	l, err := rootProperty(X, root, "_NET_CLIENT_LIST")
	if err != nil {
		return nil, err
	}

	activeWid := xproto.Window(0)
	if activeProp, err := rootProperty(X, root, "_NET_ACTIVE_WINDOW"); err == nil && len(activeProp.Value) >= 4 {
		activeWid = xproto.Window(xgb.Get32(activeProp.Value))
	}

	stackRanks := make(map[xproto.Window]int)
	if stackProp, err := rootProperty(X, root, "_NET_CLIENT_LIST_STACKING"); err == nil {
		for v := stackProp.Value; len(v) >= 4; v = v[4:] {
			wid := xproto.Window(xgb.Get32(v))
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
		if atoms[name], err = internAtom(X, name); err != nil {
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
	var ids []xproto.Window
	var cs []cookies
	for v := l.Value; len(v) >= 4; v = v[4:] {
		w := xproto.Window(xgb.Get32(v))
		ids = append(ids, w)
		cs = append(cs, cookies{
			desktop: xproto.GetProperty(X, false, w, atoms["_NET_WM_DESKTOP"], xproto.AtomCardinal, 0, 1),
			netName: xproto.GetProperty(X, false, w, atoms["_NET_WM_NAME"], atoms["UTF8_STRING"], 0, 1024),
			name:    xproto.GetProperty(X, false, w, xproto.AtomWmName, xproto.GetPropertyTypeAny, 0, 1024),
			class:   xproto.GetProperty(X, false, w, xproto.AtomWmClass, xproto.AtomString, 0, 256),
			pid:     xproto.GetProperty(X, false, w, atoms["_NET_WM_PID"], xproto.AtomCardinal, 0, 1),
			state:   xproto.GetProperty(X, false, w, atoms["_NET_WM_STATE"], xproto.GetPropertyTypeAny, 0, 32),
			hints:   xproto.GetProperty(X, false, w, xproto.AtomWmHints, xproto.GetPropertyTypeAny, 0, 9),
		})
	}
	var wins []window
	for i, c := range cs {
		// A window may be gone by now.
		d, err := c.desktop.Reply()
		if err != nil || d.Format != 32 || len(d.Value) < 4 {
			continue
		}
		w := window{id: ids[i], desktopIndex: -1}
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
				w.searchTerms, w.activeCmd, w.cwd = tree.processInfo(w.pid)
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
func sortWindows(wins []window) {
	slices.SortFunc(wins, func(a, b window) int {
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

// rofiPickWindow lets the user pick one of wins with rofi and returns
// the chosen window and the action to take. It returns errFalse if the
// user cancels.
func rofiPickWindow(wins []window, onScreen map[int]bool, curDesktop int, showAll bool, widthPct, heightPct int) (window, windowAction, error) {
	toggleAllMesg := "Alt+a all"
	if showAll {
		toggleAllMesg = "Alt+a normal"
	}
	mesg := fmt.Sprintf("Enter focus   Alt+b bring   Alt+k close   %s", toggleAllMesg)

	cmd, err := rofiCommand(widthPct, heightPct, "",
		"-dmenu", "-i", "-markup-rows", "-no-custom", "-format", "i",
		"-p", "window",
		"-mesg", mesg,
		"-kb-move-word-back", "Control+Left",
		"-kb-custom-1", "Alt+b",
		"-kb-custom-2", "Alt+k",
		"-kb-custom-3", "Alt+a",
	)
	if err != nil {
		return window{}, actionFocus, err
	}
	cmd.Stdin = strings.NewReader(strings.Join(windowRows(wins, onScreen, curDesktop), "\n"))
	out, err := cmd.Output()
	action := actionFocus
	if ee, ok := errors.AsType[*exec.ExitError](err); ok {
		switch ee.ExitCode() {
		case 10:
			action = actionBring
		case 11:
			action = actionClose
		case 12:
			action = actionToggleAll
		default:
			return window{}, actionFocus, errFalse
		}
	} else if err != nil {
		return window{}, actionFocus, errFalse
	}
	if action == actionToggleAll {
		return window{}, action, nil
	}
	n, err := strconv.Atoi(strings.TrimSpace(string(out)))
	if err != nil || n < 0 || n >= len(wins) {
		return window{}, actionFocus, fmt.Errorf("rofi: unexpected output %q", out)
	}
	return wins[n], action, nil
}

// windowRows returns the rofi rows for wins on a single formatted line:
// ! Title  — Class  Workspace  Path
// All running process metadata is included in the filter string for searching.
func windowRows(wins []window, onScreen map[int]bool, curDesktop int) []string {
	accent := accentColour()
	visibleCol := visibleColour()
	hereCol := paletteColour("green", "#859900")
	urgentBg := paletteColour("blue", "#0072d4")
	urgentFg := paletteColour("bg_0", "#ffffff")

	rows := make([]string, len(wins))
	for i, w := range wins {
		title := strings.ReplaceAll(w.title, "\n", " ")
		title = strings.TrimSpace(title)

		wsColor := accent
		if w.desktopIndex == curDesktop {
			wsColor = hereCol
		} else if w.desktopIndex >= 0 && w.desktopIndex < len(onScreen) && onScreen[w.desktopIndex] {
			wsColor = visibleCol
		}
		wsBadge := fmt.Sprintf("<span foreground='%s'>%s</span>", wsColor, html.EscapeString(w.workspace))

		var pathStr string
		if w.cwd != "" {
			home, _ := os.UserHomeDir()
			if w.cwd != home || isTerminalWindow(w) {
				pathStr = shortenHome(w.cwd)
			}
		}

		var lineParts []string
		lineParts = append(lineParts, html.EscapeString(title))
		if w.class != "" {
			lineParts = append(lineParts, fmt.Sprintf("<span fgalpha='60%%'>— %s</span>", html.EscapeString(w.class)))
		}
		lineParts = append(lineParts, wsBadge)
		if pathStr != "" {
			lineParts = append(lineParts, fmt.Sprintf("<span fgalpha='70%%'>%s</span>", html.EscapeString(pathStr)))
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

		rows[i] = fmt.Sprintf("%s\x00display\x1f%s", filterStr, displayParts)
	}
	return rows
}

func isTerminalWindow(w window) bool {
	lc := strings.ToLower(w.class)
	terminals := []string{"alacritty", "kitty", "wezterm", "term", "urxvt", "xterm", "st", "scratchpad", "ptyxis"}
	for _, t := range terminals {
		if strings.Contains(lc, t) {
			return true
		}
	}
	return commonShells[w.activeCmd] || w.activeCmd != ""
}

func shortenHome(path string) string {
	if path == "" {
		return ""
	}
	home, err := os.UserHomeDir()
	if err != nil || home == "" {
		return path
	}
	if path == home {
		return "~"
	}
	if strings.HasPrefix(path, home+"/") {
		return "~" + path[len(home):]
	}
	return path
}

// sendClientMessage sends a client message event of typ with data on the root
// window for target window w.
func sendClientMessage(X *xgb.Conn, w xproto.Window, typ string, data []uint32) error {
	root := xproto.Setup(X).DefaultScreen(X).Root
	t, err := internAtom(X, typ)
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
	return xproto.SendEventChecked(X, false, root,
		xproto.EventMaskSubstructureNotify|xproto.EventMaskSubstructureRedirect, string(ev.Bytes())).Check()
}
