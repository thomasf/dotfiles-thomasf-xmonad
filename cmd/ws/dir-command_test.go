//go:build linux

package main

import (
	"bytes"
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/peterbourgon/ff/v3/ffcli"
)

func captureOutput(f func()) string {
	r, w, _ := os.Pipe()
	oldStdout := os.Stdout
	os.Stdout = w
	defer func() {
		os.Stdout = oldStdout
	}()

	f()
	w.Close()
	var buf bytes.Buffer
	io.Copy(&buf, r)
	return buf.String()
}

func TestDirSubcommands(t *testing.T) {
	tmp := t.TempDir()
	t.Setenv("HOME", tmp)

	dirA := filepath.Join(tmp, "dirA")
	dirB := filepath.Join(tmp, "dirB")
	if err := os.MkdirAll(dirA, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(dirB, 0o755); err != nil {
		t.Fatal(err)
	}

	origWd, _ := os.Getwd()
	defer os.Chdir(origWd)

	// Test set
	if err := os.Chdir(dirA); err != nil {
		t.Fatal(err)
	}
	cmd := newDirCommand()
	if err := cmd.Parse([]string{"set", "projA"}); err != nil {
		t.Fatalf("parse failed: %v", err)
	}
	if err := cmd.Run(t.Context()); err != nil {
		t.Fatalf("run failed: %v", err)
	}

	// Test set with positional argument
	if err := os.Chdir(dirB); err != nil {
		t.Fatal(err)
	}
	cmd = newDirCommand()
	if err := cmd.Parse([]string{"set", "projB"}); err != nil {
		t.Fatalf("parse failed: %v", err)
	}
	if err := cmd.Run(t.Context()); err != nil {
		t.Fatalf("run failed: %v", err)
	}

	// Test get
	cmd = newDirCommand()
	if err := cmd.Parse([]string{"get", "projA"}); err != nil {
		t.Fatalf("parse failed: %v", err)
	}
	out := captureOutput(func() {
		if err := cmd.Run(t.Context()); err != nil {
			t.Fatalf("run failed: %v", err)
		}
	})
	if strings.TrimSpace(out) != dirA {
		t.Errorf("got %q, want %q", strings.TrimSpace(out), dirA)
	}

	// Test get with positional argument
	cmd = newDirCommand()
	if err := cmd.Parse([]string{"get", "projB"}); err != nil {
		t.Fatalf("parse failed: %v", err)
	}
	out = captureOutput(func() {
		if err := cmd.Run(t.Context()); err != nil {
			t.Fatalf("run failed: %v", err)
		}
	})
	if strings.TrimSpace(out) != dirB {
		t.Errorf("got %q, want %q", strings.TrimSpace(out), dirB)
	}

	// Test list
	cmd = newDirCommand()
	if err := cmd.Parse([]string{"list"}); err != nil {
		t.Fatalf("parse failed: %v", err)
	}
	out = captureOutput(func() {
		if err := cmd.Run(t.Context()); err != nil {
			t.Fatalf("run failed: %v", err)
		}
	})
	lines := strings.Split(strings.TrimSpace(out), "\n")
	wantLines := []string{
		"projA\t" + dirA,
		"projB\t" + dirB,
	}
	if len(lines) != len(wantLines) {
		t.Fatalf("got %d lines, want %d: %q", len(lines), len(wantLines), out)
	}
	for i := range lines {
		if lines[i] != wantLines[i] {
			t.Errorf("line %d: got %q, want %q", i, lines[i], wantLines[i])
		}
	}

	// Test unset
	cmd = newDirCommand()
	if err := cmd.Parse([]string{"unset", "projA"}); err != nil {
		t.Fatalf("parse failed: %v", err)
	}
	if err := cmd.Run(t.Context()); err != nil {
		t.Fatalf("run failed: %v", err)
	}

	// Verify projA is unset
	cmd = newDirCommand()
	if err := cmd.Parse([]string{"list"}); err != nil {
		t.Fatalf("parse failed: %v", err)
	}
	out = captureOutput(func() {
		if err := cmd.Run(t.Context()); err != nil {
			t.Fatalf("run failed: %v", err)
		}
	})
	if strings.TrimSpace(out) != "projB\t"+dirB {
		t.Errorf("after unset got %q, want %q", strings.TrimSpace(out), "projB\t"+dirB)
	}

	// Test unset with positional argument
	cmd = newDirCommand()
	if err := cmd.Parse([]string{"unset", "projB"}); err != nil {
		t.Fatalf("parse failed: %v", err)
	}
	if err := cmd.Run(t.Context()); err != nil {
		t.Fatalf("run failed: %v", err)
	}

	// Verify list is now empty
	cmd = newDirCommand()
	if err := cmd.Parse([]string{"list"}); err != nil {
		t.Fatalf("parse failed: %v", err)
	}
	out = captureOutput(func() {
		if err := cmd.Run(t.Context()); err != nil {
			t.Fatalf("run failed: %v", err)
		}
	})
	if strings.TrimSpace(out) != "" {
		t.Errorf("expected empty list, got %q", out)
	}

	// Test bare ws dir has no exec (returns NoExecError on parse)
	cmd = newDirCommand()
	err := cmd.Parse([]string{})
	var noExec ffcli.NoExecError
	if !errors.As(err, &noExec) {
		t.Errorf("expected NoExecError on bare ws dir, got %v", err)
	}
}
