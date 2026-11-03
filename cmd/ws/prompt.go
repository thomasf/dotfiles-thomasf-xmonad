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
	"unicode/utf8"

	"golang.org/x/term"
)

var errCancelled = errors.New("cancelled")

var stdin = bufio.NewReader(os.Stdin)

// readPromptLine prints prompt to stderr and reads a trimmed line from stdin.
func readPromptLine(prompt string) (string, error) {
	if prompt != "" {
		fmt.Fprint(os.Stderr, prompt)
	}
	line, err := stdin.ReadString('\n')
	return strings.TrimSpace(line), err
}

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
	rows := make([]string, len(options))
	for i, o := range options {
		rows[i] = html.EscapeString(o)
	}
	return chooseRows(msg, options, rows, 0)
}

// chooseRows is like choose, but rofi shows rows, the options in Pango
// markup, instead, with the cursor on row selected. Rows can have
// several lines.
func chooseRows(msg string, options, rows []string, selected int) (int, error) {
	if isTerminal(os.Stdin) {
		fmt.Fprintln(os.Stderr, msg)
		for i, o := range options {
			fmt.Fprintf(os.Stderr, "%3d  %s\n", i+1, o)
		}
		line, _ := readPromptLine("choice: ")
		i, err := strconv.Atoi(line)
		if err != nil || i < 1 || i > len(options) {
			return 0, errCancelled
		}
		return i - 1, nil
	}
	// Rows of several lines need another separator, and rofi makes
	// every row as high as the highest.
	height := 1
	for _, r := range rows {
		height = max(height, strings.Count(r, "\n")+1)
	}
	cmd, err := rofiCommand(90, 0, "", "-dmenu", "-i", "-no-custom", "-markup-rows", "-format", "i",
		"-selected-row", strconv.Itoa(selected), "-sep", "\x1f", "-eh", strconv.Itoa(height),
		"-p", "ws", "-mesg", html.EscapeString(msg))
	if err != nil {
		return 0, errCancelled
	}
	cmd.Stdin = strings.NewReader(strings.Join(rows, "\x1f"))
	out, err := cmd.Output()
	if err != nil {
		return 0, errCancelled
	}
	i, _, err := parseRofiIndex(out)
	if err != nil || i < 0 || i >= len(options) {
		return 0, errCancelled
	}
	return i, nil
}

// confirm shows msg and asks the user to agree to action, like
// "create", where choose asks.
func confirm(msg, action string) bool {
	if isTerminal(os.Stdin) {
		actionTitle := action
		if len(action) > 0 {
			actionTitle = strings.ToUpper(action[:1]) + action[1:]
		}
		p := fmt.Sprintf("%s\n%s? [y/N] ", msg, actionTitle)
		line, _ := readPromptLine(p)
		a := strings.ToLower(line)
		return a == "y" || a == "yes"
	}
	i, err := choose(msg, []string{action, "cancel"})
	return err == nil && i == 0
}

// padRight returns spaces to pad string s to width w runes.
func padRight(s string, w int) string {
	return strings.Repeat(" ", max(0, w-utf8.RuneCountInString(s)))
}

// --- Pango Markup Helpers ---

func pangoColor(text, color string) string {
	return fmt.Sprintf(`<span foreground="%s">%s</span>`, color, html.EscapeString(text))
}

func pangoDim(text string, alphaPct int) string {
	return fmt.Sprintf(`<span fgalpha="%d%%">%s</span>`, alphaPct, html.EscapeString(text))
}

func pangoColorDim(text, color string, alphaPct int) string {
	return fmt.Sprintf(`<span foreground="%s" fgalpha="%d%%">%s</span>`, color, alphaPct, html.EscapeString(text))
}

func pangoBold(text string) string {
	return "<b>" + html.EscapeString(text) + "</b>"
}
