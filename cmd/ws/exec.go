//go:build linux

package main

import (
	"errors"
	"maps"
	"os"
	"os/exec"
	"slices"
	"strings"
	"syscall"
)

// execLauncher replaces ws with the program in args[0] from $PATH.
// It applies any environment overrides before executing.
func execLauncher(args []string, env map[string]string) error {
	if len(args) == 0 {
		return errors.New("execLauncher: no command specified")
	}
	path, err := exec.LookPath(args[0])
	if err != nil {
		return err
	}
	return syscall.Exec(path, args, environWithOverrides(env))
}

// environWithOverrides returns os.Environ() with the specified overrides applied.
// Setting a key to an empty string unsets that variable.
func environWithOverrides(overrides map[string]string) []string {
	if len(overrides) == 0 {
		return os.Environ()
	}
	return mergeEnviron(os.Environ(), overrides)
}

func mergeEnviron(base []string, overrides map[string]string) []string {
	if len(overrides) == 0 {
		return base
	}
	applied := make(map[string]bool, len(overrides))
	var result []string
	for _, entry := range base {
		k, _, ok := strings.Cut(entry, "=")
		if !ok {
			continue
		}
		if val, exists := overrides[k]; exists {
			if !applied[k] {
				if val != "" {
					result = append(result, k+"="+val)
				}
				applied[k] = true
			}
		} else {
			result = append(result, entry)
		}
	}
	for _, k := range slices.Sorted(maps.Keys(overrides)) {
		if !applied[k] {
			if val := overrides[k]; val != "" {
				result = append(result, k+"="+val)
			}
		}
	}
	return result
}
