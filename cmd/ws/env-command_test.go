//go:build linux

package main

import (
	"errors"
	"os"
	"path/filepath"
	"testing"
)

func TestEnvFile(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	repo := newRepo(t, "dpg")
	if err := storeDir("dp", repo); err != nil {
		t.Fatal(err)
	}
	if err := storeDir("dp@other", "/elsewhere"); err != nil {
		t.Fatal(err)
	}
	wsenv := filepath.Join(home, ".config", "wsenv")
	for _, tt := range []struct {
		w    Workspace
		want string
	}{
		{"dp.1", "dpg"},
		{"dp@test.bot", "dpg@test"},
		{"dp@other", "elsewhere"},
		{"dp@nosuch.bot", "dp@nosuch"},
	} {
		got, err := envFile(tt.w)
		if want := filepath.Join(wsenv, tt.want); err != nil || got != want {
			t.Errorf("envFile(%q) = %q, %v, want %q", tt.w, got, err, want)
		}
	}

	if _, err := readEnv(filepath.Join(wsenv, "dpg")); !errors.Is(err, errFalse) {
		t.Errorf("readEnv of a missing file: error = %v, want errFalse", err)
	}

	// A worktree without a file of its own uses that of the main checkout.
	if err := os.MkdirAll(wsenv, 0o755); err != nil {
		t.Fatal(err)
	}
	for _, name := range []string{"dpg", "elsewhere"} {
		if err := os.WriteFile(filepath.Join(wsenv, name), nil, 0o644); err != nil {
			t.Fatal(err)
		}
	}
	for _, tt := range []struct {
		w    Workspace
		want string
	}{
		{"dp@test.bot", "dpg"},
		{"dp@other", "elsewhere"},
		{"dp@nosuch.bot", "dpg"},
	} {
		got, err := envFile(tt.w)
		if want := filepath.Join(wsenv, tt.want); err != nil || got != want {
			t.Errorf("envFile(%q) with main file = %q, %v, want %q", tt.w, got, err, want)
		}
	}
}
