//go:build linux

package main

import (
	"fmt"
	"os/exec"
	"strings"
)

// ghqList returns the full paths of all repositories known to ghq.
func ghqList() ([]string, error) {
	out, err := exec.Command("ghq", "list", "-p").Output()
	if err != nil {
		return nil, fmt.Errorf("ghq list: %w", err)
	}
	return splitLines(string(out)), nil
}

// ghqFind returns the repository paths for project (matching exactly).
func ghqFind(project string) ([]string, error) {
	out, err := exec.Command("ghq", "list", "-e", "-p", project).Output()
	if err != nil {
		return nil, fmt.Errorf("ghq list: %w", err)
	}
	return splitLines(string(out)), nil
}

func splitLines(s string) []string {
	var lines []string
	for line := range strings.SplitSeq(s, "\n") {
		line = strings.TrimSpace(line)
		if line != "" {
			lines = append(lines, line)
		}
	}
	return lines
}
