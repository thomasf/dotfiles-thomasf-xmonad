//go:build linux

package main

import (
	"fmt"
	"strings"
)

// splitCommand splits command line string s into arguments according to shell
// quoting rules (supporting single quotes, double quotes, and backslash escapes).
func splitCommand(s string) ([]string, error) {
	var args []string
	var cur strings.Builder
	var inQuote rune
	var escaped bool
	var hasToken bool

	for _, r := range s {
		if escaped {
			cur.WriteRune(r)
			escaped = false
			hasToken = true
			continue
		}
		if r == '\\' && inQuote != '\'' {
			escaped = true
			continue
		}
		if inQuote != 0 {
			if r == inQuote {
				inQuote = 0
			} else {
				cur.WriteRune(r)
			}
			continue
		}
		if r == '\'' || r == '"' {
			inQuote = r
			hasToken = true
			continue
		}
		if r == ' ' || r == '\t' || r == '\n' {
			if hasToken {
				args = append(args, cur.String())
				cur.Reset()
				hasToken = false
			}
			continue
		}
		cur.WriteRune(r)
		hasToken = true
	}
	if inQuote != 0 {
		return nil, fmt.Errorf("unclosed quote in %q", s)
	}
	if escaped {
		cur.WriteRune('\\')
		hasToken = true
	}
	if hasToken {
		args = append(args, cur.String())
	}
	return args, nil
}
