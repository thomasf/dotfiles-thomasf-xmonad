//go:build !linux

package main

import (
	"fmt"
	"os"
)

func main() {
	fmt.Fprintln(os.Stderr, "kbd is not supported on this platform")
	os.Exit(1)
}
