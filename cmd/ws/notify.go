//go:build linux

package main

import (
	"fmt"
	"html"
	"log"
	"os"
	"os/exec"
	"strings"
)

// notifySend sends a desktop notification or prints to stderr on a terminal.
// A notification of failure stays until dismissed, others go after a moment;
// on a terminal, failures are left to the caller to report. The notification
// replaces the one whose id is replace, unless that is "".
func notifySend(replace string, failed bool, msg string) string {
	if isTerminal(os.Stdin) {
		if !failed {
			fmt.Fprintln(os.Stderr, msg)
		}
		return ""
	}
	// The dunst rule for app shorter shows the summary and the body
	// on one line, for a short time.
	args := []string{"--print-id", "--app-name", "shorter"}
	if failed {
		args = []string{"--print-id", "--app-name", "ws", "--urgency", "critical"}
	}
	if replace != "" {
		args = append(args, "--replace-id", replace)
	}
	out, err := exec.Command("notify-send", append(args, "ws", html.EscapeString(msg))...).Output()
	if err != nil {
		log.Printf("notify-send: %v", err)
		return ""
	}
	return strings.TrimSpace(string(out))
}

// notify tells the user msg in an info notification, replacing replace (if not "").
func notify(replace, msg string) string {
	return notifySend(replace, false, msg)
}

// notifyError tells the user msg in a failure notification, replacing replace (if not "").
func notifyError(replace, msg string) string {
	return notifySend(replace, true, msg)
}
