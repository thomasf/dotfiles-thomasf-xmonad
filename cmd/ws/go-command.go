//go:build linux

package main

import (
	"context"
	"flag"
	"strings"

	"github.com/peterbourgon/ff/v3/ffcli"
)

func newGoCommand() *ffcli.Command {
	fs := flag.NewFlagSet("ws go", flag.ContinueOnError)
	cmd := &ffcli.Command{
		Name:       "go",
		ShortUsage: "ws go workspace...",
		ShortHelp:  "show workspaces across screens",
		LongHelp: `Go asks xmonad to show one or more workspaces across available screens,
creating them if new, with WS_GO / WS_GO_MANY client messages that
the xmonad configuration handles.`,
		FlagSet:   fs,
		UsageFunc: commandUsage,
	}
	cmd.Exec = func(ctx context.Context, args []string) error {
		if len(args) < 1 {
			usageError(fs)
		}
		if len(args) == 1 {
			return goWorkspace(args[0])
		}
		return sendWorkspaceMessage("WS_GO_MANY", strings.Join(args, " "))
	}
	return cmd
}

func runGo(args []string) error {
	return runFFCLI(newGoCommand(), args)
}

func newShiftCommand() *ffcli.Command {
	fs := flag.NewFlagSet("ws shift", flag.ContinueOnError)
	cmd := &ffcli.Command{
		Name:       "shift",
		ShortUsage: "ws shift [workspace]",
		ShortHelp:  "move focused window to workspace",
		LongHelp: `Shift moves the focused window to workspace, creating it if it is new,
with a WS_SHIFT client message that the xmonad configuration handles.
Without a workspace, it lets the user choose one with rofi, as ws pick -shift -empty
does.`,
		FlagSet:   fs,
		UsageFunc: commandUsage,
	}
	cmd.Exec = func(ctx context.Context, args []string) error {
		switch len(args) {
		case 0:
			return runPick([]string{"-shift", "-empty"})
		case 1:
			return shiftWorkspace(args[0])
		default:
			usageError(fs)
			return nil
		}
	}
	return cmd
}

func runShift(args []string) error {
	return runFFCLI(newShiftCommand(), args)
}

func newFillCommand() *ffcli.Command {
	fs := flag.NewFlagSet("ws fill", flag.ContinueOnError)
	cmd := &ffcli.Command{
		Name:       "fill",
		ShortUsage: "ws fill [workspace]",
		ShortHelp:  "fill all screens with workspaces from a prefix",
		LongHelp: `Fill asks xmonad to fill all available screens with workspaces sharing the
prefix of workspace (or the current workspace if none is given), with a
WS_FILL client message that the xmonad configuration handles.`,
		FlagSet:   fs,
		UsageFunc: commandUsage,
	}
	cmd.Exec = func(ctx context.Context, args []string) error {
		if len(args) > 1 {
			usageError(fs)
		}
		name := ""
		if len(args) == 1 {
			name = args[0]
		} else {
			ws, err := currentWorkspace()
			if err != nil {
				return err
			}
			name = string(ws)
		}
		return fillWorkspace(name)
	}
	return cmd
}

func runFill(args []string) error {
	return runFFCLI(newFillCommand(), args)
}

func newCleanCommand() *ffcli.Command {
	fs := flag.NewFlagSet("ws clean", flag.ContinueOnError)
	cmd := &ffcli.Command{
		Name:       "clean",
		ShortUsage: "ws clean",
		ShortHelp:  "remove empty hidden workspaces",
		LongHelp: `Clean asks xmonad to remove all empty hidden workspaces (except kept ones),
with a WS_CLEAN client message that the xmonad configuration handles.`,
		FlagSet:   fs,
		UsageFunc: commandUsage,
	}
	cmd.Exec = func(ctx context.Context, args []string) error {
		if len(args) > 0 {
			usageError(fs)
		}
		return cleanWorkspaces()
	}
	return cmd
}

func runClean(args []string) error {
	return runFFCLI(newCleanCommand(), args)
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

// fillWorkspace asks xmonad to fill all screens with workspaces from the prefix of
// workspace name, with a WS_FILL client message carrying an atom that names the
// workspace.
func fillWorkspace(name string) error {
	return sendWorkspaceMessage("WS_FILL", name)
}

// cleanWorkspaces asks xmonad to clean up empty hidden workspaces with a WS_CLEAN client message.
func cleanWorkspaces() error {
	return sendWorkspaceMessage("WS_CLEAN", "")
}
