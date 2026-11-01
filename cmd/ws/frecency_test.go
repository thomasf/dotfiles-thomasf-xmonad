//go:build linux

package main

import (
	"path/filepath"
	"slices"
	"testing"
	"time"
)

func TestFrecencyScore(t *testing.T) {
	now := time.Now()
	eRecent := FrecencyEntry{count: 2, last: now.Add(-1 * time.Hour)}
	eDay := FrecencyEntry{count: 2, last: now.Add(-10 * time.Hour)}
	eWeek := FrecencyEntry{count: 2, last: now.Add(-3 * 24 * time.Hour)}
	eMonth := FrecencyEntry{count: 2, last: now.Add(-15 * 24 * time.Hour)}
	eOld := FrecencyEntry{count: 2, last: now.Add(-40 * 24 * time.Hour)}

	if s := eRecent.score(now); s != 200 {
		t.Errorf("eRecent score = %v, want 200", s)
	}
	if s := eDay.score(now); s != 160 {
		t.Errorf("eDay score = %v, want 160", s)
	}
	if s := eWeek.score(now); s != 120 {
		t.Errorf("eWeek score = %v, want 120", s)
	}
	if s := eMonth.score(now); s != 80 {
		t.Errorf("eMonth score = %v, want 80", s)
	}
	if s := eOld.score(now); s != 40 {
		t.Errorf("eOld score = %v, want 40", s)
	}
}

func TestSortByFrecency(t *testing.T) {
	tmpDir := t.TempDir()
	historyPath := filepath.Join(tmpDir, "run_history")

	bins := []string{"alacritty", "cat", "echo", "firefox", "htop", "ls", "vi", "zsh"}

	// Record some launches
	h := loadFrecency(historyPath)
	h.record("htop")
	h.record("htop")
	h.record("firefox")

	h.sort(bins)

	// htop (score 200), firefox (score 100), then remaining in alphabetical order
	want := []string{"htop", "firefox", "alacritty", "cat", "echo", "ls", "vi", "zsh"}
	if !slices.Equal(bins, want) {
		t.Errorf("sort = %v, want %v", bins, want)
	}
}
