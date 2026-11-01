//go:build linux

package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// fileExists reports whether the file or directory at path exists.
func fileExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

// shortenHome replaces a home directory prefix in path with "~".
func shortenHome(path string) string {
	if path == "" {
		return ""
	}
	home, err := os.UserHomeDir()
	if err != nil || home == "" {
		return path
	}
	if path == home {
		return "~"
	}
	if strings.HasPrefix(path, home+"/") {
		return "~" + path[len(home):]
	}
	return path
}

// configFile returns the file name in the directory dir of
// ~/.config.
func configFile(dir, name string) (string, error) {
	if name == "" || name == "." || name == ".." || strings.ContainsRune(name, '/') {
		return "", fmt.Errorf("bad file name %q in ~/.config/%s", name, dir)
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", err
	}
	return filepath.Join(home, ".config", dir, name), nil
}

// xdgCacheFile returns the path under $XDG_CACHE_HOME/ws (or ~/.cache/ws).
func xdgCacheFile(parts ...string) string {
	cacheDir := os.Getenv("XDG_CACHE_HOME")
	if cacheDir == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return ""
		}
		cacheDir = filepath.Join(home, ".cache")
	}
	return filepath.Join(append([]string{cacheDir, "ws"}, parts...)...)
}

// writeAtomic writes data to path atomically by first writing to a temporary
// file and renaming it.
func writeAtomic(path string, data []byte, perm os.FileMode) error {
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return err
	}
	f, err := os.CreateTemp(dir, filepath.Base(path)+".tmp-*")
	if err != nil {
		return err
	}
	tmpName := f.Name()
	defer os.Remove(tmpName)

	if err := f.Chmod(perm); err != nil {
		f.Close()
		return err
	}
	if _, err := f.Write(data); err != nil {
		f.Close()
		return err
	}
	if err := f.Close(); err != nil {
		return err
	}
	return os.Rename(tmpName, path)
}
