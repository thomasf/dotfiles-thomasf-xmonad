//go:build linux

package main

import (
	"github.com/BurntSushi/xgb"
	"github.com/BurntSushi/xgb/xproto"
)

func runGo(args []string) error {
	if len(args) != 1 {
		usage()
	}
	return goWorkspace(args[0])
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
	root := xproto.Setup(X).DefaultScreen(X).Root
	a, err := internAtom(X, name)
	if err != nil {
		return err
	}
	typ, err := internAtom(X, "WS_GO")
	if err != nil {
		return err
	}
	ev := xproto.ClientMessageEvent{
		Format: 32,
		Window: root,
		Type:   typ,
		Data:   xproto.ClientMessageDataUnionData32New([]uint32{uint32(a), xproto.TimeCurrentTime, 0, 0, 0}),
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
