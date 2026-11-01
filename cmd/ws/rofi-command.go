//go:build linux

package main

import (
	"os"
	"syscall"
)

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
