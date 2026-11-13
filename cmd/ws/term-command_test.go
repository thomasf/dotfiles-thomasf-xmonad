//go:build linux

package main

import "testing"

func TestRemoteControlName(t *testing.T) {
	for _, tt := range []struct {
		w     Workspace
		taken []string
		want  string
	}{
		{"dpg@x.bot", nil, "dpg@x.bot"},
		{"dpg@x.bot", []string{"other.bot"}, "dpg@x.bot"},
		{"dpg@x.bot", []string{"dpg@x.bot"}, "dpg@x.bot+2"},
		{"dpg@x.bot", []string{"dpg@x.bot", "dpg@x.bot+2"}, "dpg@x.bot+3"},
		{"dpg@x.bot", []string{"dpg@x.bot", "dpg@x.bot+3"}, "dpg@x.bot+2"},
		{"dpg@x.bot", []string{"dpg@x.bot+2"}, "dpg@x.bot"},
		{"a/b.bot", []string{"a_b.bot"}, "a_b.bot+2"},
	} {
		if got := remoteControlName(tt.w, tt.taken); got != tt.want {
			t.Errorf("remoteControlName(%q, %q) = %q, want %q", tt.w, tt.taken, got, tt.want)
		}
	}
}
