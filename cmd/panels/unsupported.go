//go:build !linux

package main

import (
	"fmt"
	"os"
)

func main() {
	fmt.Fprintln(os.Stderr, "panels is not supported on this platform")
	os.Exit(1)
}
