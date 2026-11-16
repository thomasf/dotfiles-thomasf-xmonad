//go:build linux

package main

import (
	"bytes"
	"os/exec"
	"strings"
)

// redact filters text through delemark if the delemark executable is found in $PATH.
// If delemark is not found or encounters an error, text is returned unmodified.
func redact(text string) string {
	if text == "" {
		return ""
	}
	path, err := exec.LookPath("delemark")
	if err != nil {
		vlog("redact: preferred command %q not found in $PATH: %v; skipping redaction", "delemark", err)
		return text
	}
	cmd := exec.Command(path)
	cmd.Stdin = strings.NewReader(text)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		vlog("redact: command %q failed: %v; skipping redaction", path, err)
		return text
	}
	vlog("redact: redacted output using %s", path)
	return out.String()
}
