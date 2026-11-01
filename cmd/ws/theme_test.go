//go:build linux

package main

import "testing"

func TestPaletteColor(t *testing.T) {
	if c := fallbackPalette["cyan"]; c != "#009c8f" {
		t.Errorf("fallbackPalette[cyan] = %q, want #009c8f", c)
	}
	if c := fallbackPalette["blue"]; c != "#0072d4" {
		t.Errorf("fallbackPalette[blue] = %q, want #0072d4", c)
	}
	if c := fallbackPalette["green"]; c != "#489100" {
		t.Errorf("fallbackPalette[green] = %q, want #489100", c)
	}
	if c := fallbackPalette["bg_0"]; c != "#fbf3db" {
		t.Errorf("fallbackPalette[bg_0] = %q, want #fbf3db", c)
	}

	for _, name := range []string{"cyan", "blue", "green", "bg_0"} {
		if c := paletteColor(name); c == "" {
			t.Errorf("paletteColor(%q) returned empty string", name)
		}
	}

	if c := paletteColor("nonexistent_color_xyz"); c != "" {
		t.Errorf("paletteColor(nonexistent) = %q, want empty", c)
	}
}
