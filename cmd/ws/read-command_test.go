//go:build linux

package main

import (
	"context"
	"testing"
	"time"
)

func TestCollectEmacsSectionsNoServers(t *testing.T) {
	tmpDir := t.TempDir()
	t.Setenv("XDG_RUNTIME_DIR", tmpDir)
	ctx, cancel := context.WithTimeout(context.Background(), 500*time.Millisecond)
	defer cancel()

	sections := collectEmacsSections(ctx, commonReadFlags{visible: true}, false, false, nil)
	if len(sections) != 0 {
		t.Errorf("collectEmacsSections() with no servers got %v, want empty", sections)
	}
}

func TestWindowIsTerminal(t *testing.T) {
	tests := []struct {
		window Window
		want   bool
	}{
		{Window{class: "Alacritty"}, true},
		{Window{class: "critty"}, true},
		{Window{class: "Critty"}, true},
		{Window{class: "kitty"}, true},
		{Window{class: "Ptyxis"}, true},
		{Window{class: "Firefox"}, false},
		{Window{class: "Emacs"}, false},
	}
	for _, tc := range tests {
		if got := tc.window.isTerminal(); got != tc.want {
			t.Errorf("Window{class: %q}.isTerminal() = %v, want %v", tc.window.class, got, tc.want)
		}
	}
}
