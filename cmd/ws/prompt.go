//go:build linux

package main

import (
	"bufio"
	"errors"
	"fmt"
	"html"
	"os"
	"strconv"
	"strings"

	"golang.org/x/term"
)

var errCancelled = errors.New("cancelled")

var stdin = bufio.NewReader(os.Stdin)

// isTerminal reports whether f is a terminal. Unlike checking for a
// character device, it is false for /dev/null, which xmonad gives the
// programs it starts as standard input.
func isTerminal(f *os.File) bool {
	return term.IsTerminal(int(f.Fd()))
}

// choose shows msg and asks the user to choose one of options: on the
// terminal if standard input is one, and else with rofi. It returns
// the index of the option chosen, or errCancelled if none is.
func choose(msg string, options []string) (int, error) {
	if isTerminal(os.Stdin) {
		fmt.Fprintln(os.Stderr, msg)
		for i, o := range options {
			fmt.Fprintf(os.Stderr, "%3d  %s\n", i+1, o)
		}
		fmt.Fprint(os.Stderr, "choice: ")
		line, _ := stdin.ReadString('\n')
		i, err := strconv.Atoi(strings.TrimSpace(line))
		if err != nil || i < 1 || i > len(options) {
			return 0, errCancelled
		}
		return i - 1, nil
	}
	cmd, err := rofiCommand(50, "", "-dmenu", "-i", "-no-custom", "-format", "i", "-p", "ws", "-mesg", html.EscapeString(msg))
	if err != nil {
		return 0, errCancelled
	}
	cmd.Stdin = strings.NewReader(strings.Join(options, "\n"))
	out, err := cmd.Output()
	if err != nil {
		return 0, errCancelled
	}
	i, err := strconv.Atoi(strings.TrimSpace(string(out)))
	if err != nil || i < 0 || i >= len(options) {
		return 0, errCancelled
	}
	return i, nil
}

// confirm shows msg and asks the user to agree to action, like
// "create", where choose asks.
func confirm(msg, action string) bool {
	if isTerminal(os.Stdin) {
		fmt.Fprintf(os.Stderr, "%s\n%s%s? [y/N] ", msg, strings.ToUpper(action[:1]), action[1:])
		line, _ := stdin.ReadString('\n')
		a := strings.ToLower(strings.TrimSpace(line))
		return a == "y" || a == "yes"
	}
	i, err := choose(msg, []string{action, "cancel"})
	return err == nil && i == 0
}
