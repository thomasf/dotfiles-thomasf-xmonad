//go:build linux

package main

import (
	"fmt"
	"strings"
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
	checkArgs(fs, 0, 0)
	X, err := openX()
	if err != nil {
		return err
	}
	defer X.close()

	names, cur, err := X.desktops()
	if err != nil {
		return err
	}

	tree := readProcTree()
	wins, err := X.listWindows(names, tree)
	if err != nil {
		return err
	}
	if len(wins) == 0 {
		return nil
	}

	onScreen := X.visibleDesktops()
	showAll := *withAll
	for {
		var displayed []Window
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
			return w.focus(X)
		case actionBring:
			return w.bring(X, cur)
		case actionClose:
			return w.close(X)
		}
		return nil
	}
}

// rofiPickWindow lets the user pick one of wins with rofi and returns
// the chosen window and the action to take. It returns errFalse if the
// user cancels.
func rofiPickWindow(wins []Window, onScreen map[int]bool, curDesktop int, showAll bool, widthPct, heightPct int) (Window, WindowAction, error) {
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
		return Window{}, actionFocus, err
	}
	cmd.Stdin = strings.NewReader(strings.Join(windowRows(wins, onScreen, curDesktop), "\n"))
	out, err := cmd.Output()
	action := actionFocus
	if key, ok := rofiCustomKey(err); ok {
		switch key {
		case 1:
			action = actionBring
		case 2:
			action = actionClose
		case 3:
			action = actionToggleAll
		default:
			return Window{}, actionFocus, errFalse
		}
	} else if err != nil {
		return Window{}, actionFocus, errFalse
	}
	if action == actionToggleAll {
		return Window{}, action, nil
	}
	n, _, err := parseRofiIndex(out)
	if err != nil || n < 0 || n >= len(wins) {
		return Window{}, actionFocus, fmt.Errorf("rofi: unexpected output %q", out)
	}
	return wins[n], action, nil
}
