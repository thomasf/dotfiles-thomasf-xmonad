//go:build linux

package main

import (
	"fmt"
	"os/exec"
	"strings"
)

// ghqList returns the full paths of all repositories known to ghq.
func ghqList() ([]string, error) {
	vlog("ghq: running ghq list -p")
	if _, err := exec.LookPath("ghq"); err != nil {
		vlog("ghq: preferred command %q not found in $PATH: %v", "ghq", err)
		return nil, fmt.Errorf("ghq list: %w", err)
	}
	out, err := exec.Command("ghq", "list", "-p").Output()
	if err != nil {
		vlog("ghq: command failed: %v", err)
		return nil, fmt.Errorf("ghq list: %w", err)
	}
	lines := splitLines(string(out))
	vlog("ghq: found %d repository paths", len(lines))
	return lines, nil
}

// ghqFind returns the repository paths for project (matching exactly).
func ghqFind(project string) ([]string, error) {
	vlog("ghq: searching for project %q with ghq list -e -p", project)
	if _, err := exec.LookPath("ghq"); err != nil {
		vlog("ghq: preferred command %q not found in $PATH: %v", "ghq", err)
		return nil, fmt.Errorf("ghq list: %w", err)
	}
	out, err := exec.Command("ghq", "list", "-e", "-p", project).Output()
	if err != nil {
		vlog("ghq: command failed for project %q: %v", project, err)
		return nil, fmt.Errorf("ghq list: %w", err)
	}
	lines := splitLines(string(out))
	vlog("ghq: project %q resolved to %d path(s): %v", project, len(lines), lines)
	return lines, nil
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
