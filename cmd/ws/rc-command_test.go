//go:build linux

package main

import "testing"

func TestSplitRcName(t *testing.T) {
	for _, tt := range []struct {
		name, base string
		n          int
	}{
		{"dpg@x.bot", "dpg@x.bot", 1},
		{"dpg@x.bot+2", "dpg@x.bot", 2},
		{"dpg@x.bot+12", "dpg@x.bot", 12},
		{"dpg@x.bot+1", "dpg@x.bot+1", 1},
		{"dpg@x.bot+02", "dpg@x.bot+02", 1},
		{"c++", "c++", 1},
		{"+2", "+2", 1},
		{"teppy", "teppy", 1},
	} {
		if base, n := splitRcName(tt.name); base != tt.base || n != tt.n {
			t.Errorf("splitRcName(%q) = %q, %d, want %q, %d", tt.name, base, n, tt.base, tt.n)
		}
	}
}

func TestRcTerminalMatch(t *testing.T) {
	term := func(name string) rcTerminal {
		base, n := splitRcName(name)
		return rcTerminal{name: name, base: base, n: n}
	}
	for _, tt := range []struct {
		name, target string
		is, inList   bool
	}{
		{"dpg@x.bot", "dpg@x.bot", true, true},
		{"dpg@x.bot+2", "dpg@x.bot", true, true},
		{"dpg@x.bot+2", "dpg@x.bot+2", true, true},
		{"dpg@x.bot", "dpg@x.bot+2", false, false},
		{"dpg@x.bot+3", "dpg@x.bot+2", false, false},
		{"dpg@x.bot", "dpg@x", false, true},
		{"dpg@x.bot", "dpg", false, true},
		{"dpg.logs", "dpg", false, true},
		{"dpgx.logs", "dpg", false, false},
		{"dpg@x.bot", "dpg@xy", false, false},
		{"dpg@x.bot", "dpg@x.b", false, false},
	} {
		got := term(tt.name)
		if is := got.is(tt.target); is != tt.is {
			t.Errorf("%q.is(%q) = %v, want %v", tt.name, tt.target, is, tt.is)
		}
		if inList := got.inList(tt.target); inList != tt.inList {
			t.Errorf("%q.inList(%q) = %v, want %v", tt.name, tt.target, inList, tt.inList)
		}
	}
}

func TestTrimScreen(t *testing.T) {
	for _, tt := range []struct {
		screen string
		ansi   bool
		want   string
	}{
		{"", false, ""},
		{"   \n\t\n\n", false, ""},
		{"\n\n$ ls  \nfoo   bar\n\n\n", false, "$ ls\nfoo   bar\n"},
		{"    a\n      b\n\n    c   \n", false, "a\n  b\n\nc\n"},
		{"\tx\n\ty\n", false, "x\ny\n"},
		{"no newline", false, "no newline\n"},
		{"\n\x1b[0m\n\x1b[0;34m  ~\x1b[0m  \n\n", true, "\x1b[0;34m  ~\x1b[0m  \n"},
		{"\x1b[0m \n", true, ""},
	} {
		if got := trimScreen(tt.screen, tt.ansi); got != tt.want {
			t.Errorf("trimScreen(%q, %v) = %q, want %q", tt.screen, tt.ansi, got, tt.want)
		}
	}
}

func TestBracketedPaste(t *testing.T) {
	for _, tt := range []struct{ in, want string }{
		{"", "\x1b[200~\x1b[201~"},
		{"a\nb\n", "\x1b[200~a\nb\n\x1b[201~"},
		{"x\x1b[201~y", "\x1b[200~xy\x1b[201~"},
	} {
		if got := bracketedPaste(tt.in); got != tt.want {
			t.Errorf("bracketedPaste(%q) = %q, want %q", tt.in, got, tt.want)
		}
	}
}
