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

	sections := collectEmacsSections(ctx, true, false, nil)
	if len(sections) != 0 {
		t.Errorf("collectEmacsSections() with no servers got %v, want empty", sections)
	}
}
