//go:build linux

package main

import (
	"os"
	"strings"
	"testing"

	"github.com/jezek/xgb/xproto"
)

func TestWindowRows(t *testing.T) {
	wins := []Window{
		{
			id:           1,
			title:        "a <b>\nc",
			class:        "Alacritty",
			workspace:    "x&y",
			desktopIndex: 0,
			activeCmd:    "claude",
			searchTerms:  []string{"claude", "main.go"},
			cwd:          "/tmp/project",
		},
		{
			id:           2,
			title:        "Urgent Alert",
			class:        "Slack",
			workspace:    "chat",
			desktopIndex: 1,
			urgent:       true,
		},
		{
			id:           3,
			title:        "Focused Terminal",
			class:        "Alacritty",
			workspace:    "here-ws",
			desktopIndex: 2,
			active:       true,
			cwd:          "/home/t/src/dotfiles/xmonad",
		},
	}
	onScreen := map[int]bool{0: true}
	curDesktop := 2
	rows := windowRows(wins, onScreen, curDesktop)
	if len(rows) != 3 {
		t.Fatalf("windowRows returned %d rows, want 3", len(rows))
	}

	// 1. Regular window on other screen with cwd
	{
		row := rows[0]
		parts := strings.Split(row, "\x00")
		if len(parts) < 2 {
			t.Fatalf("row missing \\0 delimiter: %q", row)
		}
		filterPart := parts[0]
		for _, expected := range []string{"a <b>\nc", "Alacritty", "x&y", "claude", "main.go", "/tmp/project"} {
			if !strings.Contains(filterPart, expected) {
				t.Errorf("filterPart %q does not contain %q", filterPart, expected)
			}
		}

		opts := strings.Split(parts[1], "\x1f")
		optMap := make(map[string]string)
		for i := 0; i+1 < len(opts); i += 2 {
			optMap[opts[i]] = opts[i+1]
		}
		display := optMap["display"]
		if !strings.Contains(display, "a &lt;b&gt; c") {
			t.Errorf("display %q does not contain escaped title", display)
		}
		if !strings.Contains(display, "— Alacritty") {
			t.Errorf("display %q does not contain class", display)
		}
		// Brackets removed around workspace name
		if strings.Contains(display, "[x&amp;y]") {
			t.Errorf("display %q should not contain brackets around workspace name", display)
		}
		if !strings.Contains(display, ">x&amp;y</span>") {
			t.Errorf("display %q missing workspace name", display)
		}
		// Path after workspace name
		if !strings.Contains(display, "/tmp/project") {
			t.Errorf("display %q missing path /tmp/project", display)
		}
	}

	// 2. Urgent window (Slack, no cwd shown)
	{
		row := rows[1]
		if strings.Contains(row, "\x1furgent\x1ftrue") {
			t.Errorf("urgent row %q should not contain \\x1furgent\\x1ftrue", row)
		}
		parts := strings.Split(row, "\x00")
		filterPart := parts[0]
		if !strings.Contains(filterPart, "urgent") || !strings.Contains(filterPart, "!") {
			t.Errorf("urgent filterPart %q missing 'urgent' or '!'", filterPart)
		}
		opts := strings.Split(parts[1], "\x1f")
		optMap := make(map[string]string)
		for i := 0; i+1 < len(opts); i += 2 {
			optMap[opts[i]] = opts[i+1]
		}
		display := optMap["display"]
		blue := paletteColor("blue")
		bg0 := paletteColor("bg_0")
		if !strings.Contains(display, "<b>!</b>") || !strings.Contains(display, blue) || !strings.Contains(display, bg0) {
			t.Errorf("urgent display %q missing inverted bold ! badge (blue: %s, bg0: %s)", display, blue, bg0)
		}
		if strings.Contains(display, "[chat]") {
			t.Errorf("urgent display %q should not contain brackets around workspace name", display)
		}
		if !strings.Contains(display, ">chat</span>") {
			t.Errorf("urgent display %q missing workspace name", display)
		}
	}

	// 3. Current workspace and active window with cwd
	{
		row := rows[2]
		parts := strings.Split(row, "\x00")
		filterPart := parts[0]
		if !strings.Contains(filterPart, "here") {
			t.Errorf("filterPart %q for current desktop window missing 'here'", filterPart)
		}
		opts := strings.Split(parts[1], "\x1f")
		optMap := make(map[string]string)
		for i := 0; i+1 < len(opts); i += 2 {
			optMap[opts[i]] = opts[i+1]
		}
		display := optMap["display"]
		// Workspace name is kept without brackets, and colored green
		if strings.Contains(display, "[here-ws]") {
			t.Errorf("current desktop display %q should not contain brackets around workspace name", display)
		}
		if !strings.Contains(display, ">here-ws</span>") {
			t.Errorf("current desktop display %q missing 'here-ws' workspace name", display)
		}
		green := paletteColor("green")
		if !strings.Contains(display, green) {
			t.Errorf("current desktop display %q missing green color %q", display, green)
		}
		// No (current) text
		if strings.Contains(display, "(current)") {
			t.Errorf("active window display %q should not contain (current)", display)
		}
		// Path displayed after workspace name
		shortenedPath := shortenHome("/home/t/src/dotfiles/xmonad")
		if !strings.Contains(display, shortenedPath) {
			t.Errorf("current desktop display %q missing path %q", display, shortenedPath)
		}
		if optMap["active"] != "" {
			t.Errorf("active row %q should not have active metadata", row)
		}
	}
}

func TestSortWindows(t *testing.T) {
	wins := []Window{
		{id: 1, title: "active window", active: true, stackRank: 10},
		{id: 2, title: "oldest window", stackRank: 1},
		{id: 3, title: "urgent window", urgent: true, stackRank: 5},
		{id: 4, title: "previous window", stackRank: 9},
	}

	sortWindows(wins)

	// Expected order:
	// 1. urgent window (id: 3)
	// 2. previous window (id: 4, MRU rank 9)
	// 3. oldest window (id: 2, MRU rank 1)
	// 4. active window (id: 1, active sorted to bottom)
	expectedIDs := []xproto.Window{3, 4, 2, 1}
	for i, exp := range expectedIDs {
		if wins[i].id != exp {
			t.Errorf("wins[%d].id = %d, want %d (%s)", i, wins[i].id, exp, wins[i].title)
		}
	}
}

func TestProcessTree(t *testing.T) {
	tree := &ProcTree{
		children: map[int][]int{
			100: {101},
			101: {102, 103},
			102: {104}, // claude spawns gopls in another dir
			200: {201}, // emacs spawns gopls in another dir
		},
		nodes: map[int]*ProcNode{
			100: {pid: 100, ppid: 1, comm: "alacritty", cwd: "/home/t"},
			101: {pid: 101, ppid: 100, comm: "bash", cmdline: []string{"bash"}, cwd: "/home/t/src/dotfiles/xmonad"},
			102: {pid: 102, ppid: 101, comm: "node", cmdline: []string{"node", "/usr/bin/claude", "--resume"}, cwd: "/home/t/src/dotfiles/xmonad"},
			103: {pid: 103, ppid: 101, comm: "git", cmdline: []string{"git", "status"}},
			104: {pid: 104, ppid: 102, comm: "gopls", cmdline: []string{"gopls"}, cwd: "/tmp/other/module"},

			200: {pid: 200, ppid: 1, comm: "emacs", cwd: "/home/t/src/dotfiles/xmonad"},
			201: {pid: 201, ppid: 200, comm: "gopls", cmdline: []string{"gopls"}, cwd: "/tmp/other/module"},
		},
	}

	// 1. Alacritty should use shell's cwd (/home/t/src/dotfiles/xmonad), NOT gopls (/tmp/other/module)
	desc := tree.descendants(100)
	if len(desc) != 4 {
		t.Fatalf("descendants count = %d, want 4", len(desc))
	}
	d := tree.details(100)
	termsStr := strings.Join(d.searchTerms, " ")
	for _, want := range []string{"alacritty", "bash", "node", "claude", "git", "status", "xmonad", "dotfiles"} {
		if !strings.Contains(termsStr, want) {
			t.Errorf("terms %q does not contain %q", termsStr, want)
		}
	}
	if strings.Contains(termsStr, "/tmp/other/module") || strings.Contains(termsStr, "other") {
		t.Errorf("terms %q should not contain child daemon cwd", termsStr)
	}
	if d.activeCmd == "" {
		t.Errorf("activeCmd should not be empty")
	}
	if d.cwd != "/home/t/src/dotfiles/xmonad" {
		t.Errorf("terminal cwd = %q, want %q", d.cwd, "/home/t/src/dotfiles/xmonad")
	}

	// 2. Emacs (GUI app) should use root cwd (/home/t/src/dotfiles/xmonad), NOT gopls (/tmp/other/module)
	emacsCwd := tree.details(200).cwd
	if emacsCwd != "/home/t/src/dotfiles/xmonad" {
		t.Errorf("emacs cwd = %q, want %q", emacsCwd, "/home/t/src/dotfiles/xmonad")
	}
}

func TestShortenHome(t *testing.T) {
	home, err := os.UserHomeDir()
	if err != nil || home == "" {
		t.Skip("no home dir")
	}
	if got := shortenHome(home); got != "~" {
		t.Errorf("shortenHome(%q) = %q, want ~", home, got)
	}
	if got := shortenHome(home + "/src/dotfiles"); got != "~/src/dotfiles" {
		t.Errorf("shortenHome(%q) = %q, want ~/src/dotfiles", home+"/src/dotfiles", got)
	}
	if got := shortenHome("/tmp/foo"); got != "/tmp/foo" {
		t.Errorf("shortenHome(/tmp/foo) = %q, want /tmp/foo", got)
	}
}

func TestTextProperty(t *testing.T) {
	latin1 := &xproto.GetPropertyReply{Type: xproto.AtomString, Value: []byte("caf\xe9")}
	if got := textProperty(latin1); got != "café" {
		t.Errorf("textProperty(STRING) = %q, want %q", got, "café")
	}
}

func TestScreenAt(t *testing.T) {
	mons := []Monitor{
		{"HDMI-0", Rect{0, 2160, 3840, 2160}},
		{"DP-0", Rect{3840, 2160, 3840, 2160}},
		{"DP-2", Rect{3840, 0, 3840, 2160}},
	}
	// DP-0 is split into two screens side by side, DP-2 into two stacked.
	origins := []Point{{0, 2160}, {3842, 2162}, {5762, 2162}, {3840, 0}, {3840, 1080}, {3842, 2162}}
	for _, tt := range []struct {
		o    Point
		want Rect
		mon  string
	}{
		{Point{0, 2160}, Rect{0, 2160, 3840, 2160}, "HDMI-0"},
		{Point{3842, 2162}, Rect{3842, 2162, 1920, 2158}, "DP-0"},
		{Point{5762, 2162}, Rect{5762, 2162, 1918, 2158}, "DP-0"},
		{Point{3840, 0}, Rect{3840, 0, 3840, 1080}, "DP-2"},
		{Point{3840, 1080}, Rect{3840, 1080, 3840, 1080}, "DP-2"},
	} {
		got, m, err := screenAt(tt.o, origins, mons)
		if err != nil || got != tt.want || m.name != tt.mon {
			t.Errorf("screenAt(%v) = %v, %s, %v, want %v, %s", tt.o, got, m.name, err, tt.want, tt.mon)
		}
	}
	if _, _, err := screenAt(Point{99999, 0}, origins, mons); err == nil {
		t.Error("screenAt outside every monitor succeeded")
	}
}
