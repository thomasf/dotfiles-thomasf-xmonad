//go:build linux

package main

import (
	"bytes"
	"encoding/base64"
	"errors"
	"fmt"
	"os/exec"
	"strings"
)

func runEmacs(args []string) error {
	fs := newFlagSet("emacs", "[-noheader] region | visible", `
Emacs prints text from the Emacs of the workspace, the server named
after the file name of its prefix (see ws name -p), for the workspace
that contextWorkspace resolves: that of $WS_WORKSPACE, which ws term
sets, or else the current one.

    region     the marked text: the active region of the selected
               window, else of another window on a visible frame,
               after a line naming its file and lines
    visible    the text of every window on the visible frames, each
               after a line naming its buffer

It exits with status 1 if no text is marked or the workspace has no
Emacs.
`)
	noHeader := fs.Bool("noheader", false, "print the region without the line naming its file and lines")
	fs.Parse(args)
	checkArgs(fs, 1, 1)
	var expr string
	switch fs.Arg(0) {
	case "region":
		expr = fmt.Sprintf("(my-region-text %s)", map[bool]string{true: "t", false: "nil"}[*noHeader])
	case "visible":
		expr = "(my-visible-text)"
	default:
		usageError(fs)
	}
	w, err := contextWorkspace()
	if err != nil {
		return err
	}
	text, err := emacsEval(fileName(w.prefix()), expr)
	if err != nil {
		return err
	}
	fmt.Print(text)
	if text != "" && !strings.HasSuffix(text, "\n") {
		fmt.Println()
	}
	return nil
}

// emacsEval evaluates expr, which must return a string, in the Emacs
// server named server and returns the string. The string travels base64
// encoded, as emacsclient prints values as Lisp, quoted and escaped.
func emacsEval(server, expr string) (string, error) {
	expr = fmt.Sprintf("(base64-encode-string (encode-coding-string %s 'utf-8 t) t)", expr)
	// --alternate-editor=false keeps emacsclient from starting a daemon
	// when there is no server, as an empty $ALTERNATE_EDITOR would.
	cmd := exec.Command("emacsclient", "--alternate-editor=false", "-s", server, "-e", expr)
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		if !errors.As(err, new(*exec.ExitError)) {
			return "", err
		}
		msg := strings.TrimPrefix(strings.TrimSpace(stderr.String()), "*ERROR*: ")
		if msg == "" {
			msg = "no Emacs server " + server
		}
		return "", errors.New("emacs: " + msg)
	}
	b, err := base64.StdEncoding.DecodeString(string(bytes.Trim(bytes.TrimSpace(out), `"`)))
	if err != nil {
		return "", fmt.Errorf("emacsclient: %w", err)
	}
	return string(b), nil
}
