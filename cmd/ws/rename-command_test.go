//go:build linux

package main

import "testing"

func TestRenameCandidates(t *testing.T) {
	candidates := renameCandidates("disco")
	if len(candidates) == 0 {
		t.Fatal("expected non-empty candidates")
	}
	if candidates[0] != "disco" {
		t.Errorf("expected current workspace at index 0, got %q", candidates[0])
	}
}
