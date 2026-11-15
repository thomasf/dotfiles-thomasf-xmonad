//go:build linux

package main

import (
	"context"
	"os"
	"syscall"

	"github.com/peterbourgon/ff/v3/ffcli"
)

func newRofiCommand() *ffcli.Command {
	return &ffcli.Command{
		Name:       "rofi",
		ShortUsage: "ws rofi [-width pct] [-height pct] [rofi arguments]",
		ShortHelp:  "run rofi placed on the current xmonad screen",
		LongHelp:   `Rofi runs rofi with arguments, placed on the current xmonad screen.`,
		UsageFunc:  commandUsage,
		Exec: func(ctx context.Context, args []string) error {
			return runRofi(args)
		},
	}
}

// runRofi runs rofi with args, placed on the current xmonad screen. It
// replaces ws with rofi.
func runRofi(args []string) error {
	widthPct, heightPct, args, err := rofiSizeArgs(args)
	if err != nil {
		return err
	}
	path, err := rofiPath()
	if err != nil {
		return err
	}
	argv := append([]string{"rofi"}, rofiPlacement(widthPct, heightPct, "")...)
	return syscall.Exec(path, append(argv, args...), os.Environ())
}
