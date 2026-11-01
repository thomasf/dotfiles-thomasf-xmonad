//go:build linux

package main

import (
	"os"
	"path/filepath"
	"slices"
	"testing"
	"time"
)

func TestPathBinaries(t *testing.T) {
	tmpDir := t.TempDir()
	binDir1 := filepath.Join(tmpDir, "bin1")
	binDir2 := filepath.Join(tmpDir, "bin2")
	cacheFile := filepath.Join(tmpDir, "cache", "run_cache")

	if err := os.MkdirAll(binDir1, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(binDir2, 0o755); err != nil {
		t.Fatal(err)
	}

	// Create executable files in bin1
	mustWriteExec(t, filepath.Join(binDir1, "foo"), true)
	mustWriteExec(t, filepath.Join(binDir1, "bar"), false) // not executable
	mustWriteExec(t, filepath.Join(binDir1, ".hidden"), true)
	mustWriteExec(t, filepath.Join(binDir1, "rofi-theme-selector"), true) // blacklisted

	// Create executable files in bin2
	mustWriteExec(t, filepath.Join(binDir2, "baz"), true)
	mustWriteExec(t, filepath.Join(binDir2, "foo"), true) // duplicate

	pathEnv := binDir1 + ":" + binDir2

	// Initial scan should find foo and baz, and write cache
	bins, err := pathBinariesFrom(pathEnv, cacheFile)
	if err != nil {
		t.Fatalf("pathBinariesFrom: %v", err)
	}
	want := []string{"baz", "foo"}
	if !slices.Equal(bins, want) {
		t.Fatalf("pathBinariesFrom = %v, want %v", bins, want)
	}

	// Verify cache file was created
	if _, err := os.Stat(cacheFile); err != nil {
		t.Fatalf("cache file not created: %v", err)
	}

	// Next read should use cache
	cachedBins, ok := readCachedBinaries(cacheFile, pathEnv)
	if !ok {
		t.Fatalf("readCachedBinaries returned false, expected true")
	}
	if !slices.Equal(cachedBins, want) {
		t.Fatalf("readCachedBinaries = %v, want %v", cachedBins, want)
	}

	// Modify binDir1 after cache modification time to invalidate cache
	time.Sleep(10 * time.Millisecond)
	mustWriteExec(t, filepath.Join(binDir1, "newbin"), true)
	futureTime := time.Now().Add(time.Second)
	_ = os.Chtimes(binDir1, futureTime, futureTime)

	// Cache should now report invalid
	if _, ok := readCachedBinaries(cacheFile, pathEnv); ok {
		t.Fatalf("readCachedBinaries returned true after dir modified, expected false")
	}

	// Rescan should now include newbin
	bins2, err := pathBinariesFrom(pathEnv, cacheFile)
	if err != nil {
		t.Fatalf("pathBinariesFrom: %v", err)
	}
	want2 := []string{"baz", "foo", "newbin"}
	if !slices.Equal(bins2, want2) {
		t.Fatalf("pathBinariesFrom after invalidation = %v, want %v", bins2, want2)
	}
}

func mustWriteExec(t *testing.T, path string, executable bool) {
	t.Helper()
	mode := os.FileMode(0o644)
	if executable {
		mode = 0o755
	}
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"), mode); err != nil {
		t.Fatal(err)
	}
}
