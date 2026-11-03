//go:build linux

package main

func runGo(args []string) error {
	fs := newFlagSet("go", "workspace", `
Go asks xmonad to show a workspace, creating it if it is new, with a
WS_GO client message that the xmonad configuration handles.
`)
	fs.Parse(args)
	checkArgs(fs, 1, 1)
	return goWorkspace(fs.Arg(0))
}

func runShift(args []string) error {
	fs := newFlagSet("shift", "[workspace]", `
Shift moves the focused window to workspace, creating it if it is new,
with a WS_SHIFT client message that the xmonad configuration handles.
Without a workspace, it lets the user choose one with rofi, as ws pick -shift -empty
does.
`)
	fs.Parse(args)
	switch fs.NArg() {
	case 0:
		return runPick([]string{"-shift", "-empty"})
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
	return sendWorkspaceMessage("WS_GO", name)
}

// shiftWorkspace asks xmonad to move the focused window to workspace name,
// creating it if it is new, with a WS_SHIFT client message carrying an atom
// that names the workspace.
func shiftWorkspace(name string) error {
	return sendWorkspaceMessage("WS_SHIFT", name)
}

func runFill(args []string) error {
	fs := newFlagSet("fill", "[workspace]", `
Fill asks xmonad to fill all available screens with workspaces sharing the
prefix of workspace (or the current workspace if none is given), with a
WS_FILL client message that the xmonad configuration handles.
`)
	fs.Parse(args)
	checkArgs(fs, 0, 1)
	name := ""
	if fs.NArg() == 1 {
		name = fs.Arg(0)
	} else {
		ws, err := currentWorkspace()
		if err != nil {
			return err
		}
		name = string(ws)
	}
	return fillWorkspace(name)
}

// fillWorkspace asks xmonad to fill all screens with workspaces from the prefix of
// workspace name, with a WS_FILL client message carrying an atom that names the
// workspace.
func fillWorkspace(name string) error {
	return sendWorkspaceMessage("WS_FILL", name)
}

func runClean(args []string) error {
	fs := newFlagSet("clean", "", `
Clean asks xmonad to remove all empty hidden workspaces (except kept ones),
with a WS_CLEAN client message that the xmonad configuration handles.
`)
	fs.Parse(args)
	checkArgs(fs, 0, 0)
	return cleanWorkspaces()
}

// cleanWorkspaces asks xmonad to clean up empty hidden workspaces with a WS_CLEAN client message.
func cleanWorkspaces() error {
	return sendWorkspaceMessage("WS_CLEAN", "")
}
