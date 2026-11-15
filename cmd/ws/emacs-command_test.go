//go:build linux

package main

import (
	"net"
	"os"
	"path/filepath"
	"slices"
	"testing"
)

func TestResolveEmacsTarget(t *testing.T) {
	for _, tt := range []struct {
		target string
		want   string
	}{
		{"dotfiles", "dotfiles"},
		{"dotfiles.emacs", "dotfiles"},
		{"proj@feat.bot", "proj@feat"},
		{"my/project", "my_project"},
	} {
		got, err := resolveEmacsTarget(tt.target)
		if err != nil {
			t.Fatalf("resolveEmacsTarget(%q) error: %v", tt.target, err)
		}
		if got != tt.want {
			t.Errorf("resolveEmacsTarget(%q) = %q, want %q", tt.target, got, tt.want)
		}
	}
}

func TestRunningEmacsServers(t *testing.T) {
	tmpDir := t.TempDir()
	t.Setenv("XDG_RUNTIME_DIR", tmpDir)

	emacsDir := filepath.Join(tmpDir, "emacs")
	if err := os.MkdirAll(emacsDir, 0700); err != nil {
		t.Fatal(err)
	}

	// Create fake UNIX sockets
	sock1 := filepath.Join(emacsDir, "alpha")
	l1, err := net.Listen("unix", sock1)
	if err != nil {
		t.Fatal(err)
	}
	defer l1.Close()

	sock2 := filepath.Join(emacsDir, "beta")
	l2, err := net.Listen("unix", sock2)
	if err != nil {
		t.Fatal(err)
	}
	defer l2.Close()

	// Also create a regular file, which should be ignored
	regular := filepath.Join(emacsDir, "not-a-socket")
	if err := os.WriteFile(regular, []byte("test"), 0600); err != nil {
		t.Fatal(err)
	}

	servers := runningEmacsServers()
	want := []string{"alpha", "beta"}
	if !slices.Equal(servers, want) {
		t.Errorf("runningEmacsServers() = %v, want %v", servers, want)
	}
}

func TestOrderEmacsServers(t *testing.T) {
	t.Setenv("WS_WORKSPACE", "ws.bot")
	servers := []string{"alpha", "ws", "beta"}
	got := orderEmacsServers(servers)
	if len(got) != 3 || got[0] != "ws" {
		t.Errorf("orderEmacsServers(%v) = %v, want ws first", servers, got)
	}
}

func TestRunEmacsListNoServers(t *testing.T) {
	tmpDir := t.TempDir()
	t.Setenv("XDG_RUNTIME_DIR", tmpDir)
	// Empty directory, no servers
	err := runEmacsList([]string{"-global"})
	if err == nil || err.Error() != "emacs: no running Emacs servers found" {
		t.Errorf("runEmacsList() err = %v, want 'no running Emacs servers found'", err)
	}
}
