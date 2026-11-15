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

func TestRunEmacsReadNoServers(t *testing.T) {
	tmpDir := t.TempDir()
	t.Setenv("XDG_RUNTIME_DIR", tmpDir)
	// Empty directory, -visible returns nil (no visible servers to show)
	err := runEmacsBuffers(nil, "", commonReadFlags{visible: true})
	if err != nil {
		t.Errorf("runEmacsBuffers(visible) err = %v, want nil", err)
	}

	// -global returns error
	err = runEmacsBuffers(nil, "", commonReadFlags{global: true})
	if err == nil || err.Error() != "emacs: no running Emacs servers found" {
		t.Errorf("runEmacsBuffers(global) err = %v, want 'no running Emacs servers found'", err)
	}

	// target returns error
	err = runEmacsBuffers([]string{"dotfiles"}, "", commonReadFlags{})
	if err == nil || err.Error() != "emacs: no running Emacs servers found for dotfiles" {
		t.Errorf("runEmacsBuffers(target) err = %v, want 'no running Emacs servers found for dotfiles'", err)
	}
}

func TestEmacsTextExpr(t *testing.T) {
	visibleExpr := emacsTextExpr(false, 0, false)
	if !slices.Contains([]byte(visibleExpr), byte('(')) {
		t.Errorf("emacsTextExpr returned invalid expression")
	}

	fullExpr := emacsTextExpr(true, 0, false)
	if !slices.Contains([]byte(fullExpr), byte('(')) {
		t.Errorf("emacsTextExpr(full) returned invalid expression")
	}

	linesExpr := emacsTextExpr(false, 20, false)
	if !slices.Contains([]byte(linesExpr), byte('(')) {
		t.Errorf("emacsTextExpr(lines) returned invalid expression")
	}
}

func TestVisibleEmacsServers(t *testing.T) {
	t.Setenv("WS_WORKSPACE", "ws.bot")
	servers := []string{"alpha", "ws", "beta"}
	got := visibleEmacsServers(servers)
	// In test environment without X, contextWorkspace is ws.bot -> prefix "ws"
	if len(got) != 1 || got[0] != "ws" {
		t.Errorf("visibleEmacsServers(%v) = %v, want [ws]", servers, got)
	}
}
