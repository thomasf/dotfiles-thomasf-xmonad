//go:build linux

package main

import (
	"github.com/jezek/xgb"
	"github.com/jezek/xgb/xproto"
)

func runGo(args []string) error {
	fs := newFlagSet("go", "workspace", `
Go asks xmonad to show a workspace, creating it if it is new, with a
WS_GO client message that the xmonad configuration handles.
`)
	fs.Parse(args)
	if fs.NArg() != 1 {
		usageError(fs)
	}
	return goWorkspace(fs.Arg(0))
}

func runShift(args []string) error {
	fs := newFlagSet("shift", "[workspace]", `
Shift moves the focused window to workspace, creating it if it is new,
with a WS_SHIFT client message that the xmonad configuration handles.
Without a workspace, it lets the user choose one with rofi, as ws pick -shift
does.
`)
	fs.Parse(args)
	switch fs.NArg() {
	case 0:
		return runPick([]string{"-shift"})
	case 1:
		return shiftWorkspace(fs.Arg(0))
	default:
		usageError(fs)
		return nil
	}
}

// goWorkspace asks xmonad to show workspace name, creating it if it is
// new, with a WS_GO client message carrying an atom that names the
// workspace. wsGoEventHook in the xmonad configuration handles it as the
// workspace prompt did: it also starts the workspace action of an empty
// workspace and removes the workspace left if it is empty. EWMH's
// _NET_CURRENT_DESKTOP does none of that and can't create workspaces.
// An xmonad without the hook ignores the message.
func goWorkspace(name string) error {
	X, err := xgb.NewConn()
	if err != nil {
		return err
	}
	defer X.Close()
	a, err := internAtom(X, name)
	if err != nil {
		return err
	}
	return sendXmonad(X, "WS_GO", uint32(a))
}

// shiftWorkspace asks xmonad to move the focused window to workspace name,
// creating it if it is new, with a WS_SHIFT client message carrying an atom
// that names the workspace.
func shiftWorkspace(name string) error {
	X, err := xgb.NewConn()
	if err != nil {
		return err
	}
	defer X.Close()
	a, err := internAtom(X, name)
	if err != nil {
		return err
	}
	return sendXmonad(X, "WS_SHIFT", uint32(a))
}

// renameWorkspace asks xmonad to rename the current workspace to name
// with a WS_RENAME client message carrying an atom that names the new workspace.
func renameWorkspace(name string) error {
	X, err := xgb.NewConn()
	if err != nil {
		return err
	}
	defer X.Close()
	a, err := internAtom(X, name)
	if err != nil {
		return err
	}
	return sendXmonad(X, "WS_RENAME", uint32(a))
}

// sendXmonad sends xmonad a client message of type typ on the root
// window, with datum as its first datum.
func sendXmonad(X *xgb.Conn, typ string, datum uint32) error {
	root := xproto.Setup(X).DefaultScreen(X).Root
	t, err := internAtom(X, typ)
	if err != nil {
		return err
	}
	ev := xproto.ClientMessageEvent{
		Format: 32,
		Window: root,
		Type:   t,
		Data:   xproto.ClientMessageDataUnionData32New([]uint32{datum, xproto.TimeCurrentTime, 0, 0, 0}),
	}
	return xproto.SendEventChecked(X, false, root,
		xproto.EventMaskSubstructureNotify|xproto.EventMaskSubstructureRedirect, string(ev.Bytes())).Check()
}

func internAtom(X *xgb.Conn, name string) (xproto.Atom, error) {
	a, err := xproto.InternAtom(X, false, uint16(len(name)), name).Reply()
	if err != nil {
		return 0, err
	}
	return a.Atom, nil
}
