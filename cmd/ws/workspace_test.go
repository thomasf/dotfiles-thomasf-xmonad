//go:build linux

package main

import (
	"errors"
	"path/filepath"
	"testing"
)

func TestWorkspace(t *testing.T) {
	for _, tt := range []struct {
		w                                 Workspace
		prefix, project, worktree, suffix string
		hasSuffix                         bool
	}{
		{"hello", "hello", "hello", "", "", false},
		{"hello.1", "hello", "hello", "", "1", true},
		{"hello@test", "hello@test", "hello", "test", "", false},
		{"hello@test.bot", "hello@test", "hello", "test", "bot", true},
		{"tl.log.x", "tl", "tl", "", "log.x", true},
		{"a@b@c.d", "a@b@c", "a", "b@c", "d", true},
	} {
		suffix, hasSuffix := tt.w.suffix()
		if got := tt.w.prefix(); got != tt.prefix {
			t.Errorf("%q.prefix() = %q, want %q", tt.w, got, tt.prefix)
		}
		if got := tt.w.project(); got != tt.project {
			t.Errorf("%q.project() = %q, want %q", tt.w, got, tt.project)
		}
		if got := tt.w.worktree(); got != tt.worktree {
			t.Errorf("%q.worktree() = %q, want %q", tt.w, got, tt.worktree)
		}
		if suffix != tt.suffix || hasSuffix != tt.hasSuffix {
			t.Errorf("%q.suffix() = %q, %v, want %q, %v", tt.w, suffix, hasSuffix, tt.suffix, tt.hasSuffix)
		}
	}
}

func TestWorktreeDir(t *testing.T) {
	repo := newRepo(t, "hello")
	wt := repo + "@test"
	for _, tt := range []struct{ dir, want string }{
		{repo, wt},
		{filepath.Join(repo, "sub"), filepath.Join(wt, "sub")},
		{wt, wt},
	} {
		got, err := worktreeDir(tt.dir, "test")
		if err != nil || got != tt.want {
			t.Errorf("worktreeDir(%q, test) = %q, %v, want %q", tt.dir, got, err, tt.want)
		}
	}
	if _, err := worktreeDir(repo, "nosuch"); !errors.Is(err, errNoDir) {
		t.Errorf("worktreeDir(%q, nosuch) error = %v, want errNoDir", repo, err)
	}
}

func TestWorkspaceDirStored(t *testing.T) {
	t.Setenv("HOME", t.TempDir())
	repo := newRepo(t, "dpg")
	sub := filepath.Join(repo, "sub")
	if err := storeDir("dp", sub); err != nil {
		t.Fatal(err)
	}
	if err := storeDir("dp@other", "/elsewhere"); err != nil {
		t.Fatal(err)
	}
	for _, tt := range []struct {
		w    Workspace
		want string
	}{
		{"dp", sub},
		{"dp.1", sub},
		{"dp@test.bot", filepath.Join(repo+"@test", "sub")},
		{"dp@other.bot", "/elsewhere"},
	} {
		got, err := workspaceDir(tt.w, noPick)
		if err != nil || got != tt.want {
			t.Errorf("workspaceDir(%q) = %q, %v, want %q", tt.w, got, err, tt.want)
		}
	}

	if err := storeDir("dp", ""); err != nil {
		t.Fatal(err)
	}
	if got, err := storedDir("dp"); got != "" || err != nil {
		t.Errorf("storedDir(dp) after removal = %q, %v, want \"\", nil", got, err)
	}
	if err := storeDir("dp", ""); err != nil {
		t.Errorf("removing a missing stored directory: %v", err)
	}
	if _, err := storedDir("../x"); err == nil {
		t.Error("storedDir(../x) succeeded")
	}
}
