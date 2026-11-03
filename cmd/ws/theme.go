//go:build linux

package main

import (
	"encoding/json"
	"os"
	"sync"
)

// fallbackPalette is Selenized light (jan-warchol/selenized), used
// when ~/.config/theme/palette.json cannot be read or does not contain a color.
var fallbackPalette = map[string]string{
	"bg_0":    "#fbf3db",
	"bg_1":    "#ece3cc",
	"bg_2":    "#d5cdb6",
	"dim_0":   "#909995",
	"dim_1":   "#718081",
	"fg_0":    "#53676d",
	"fg_1":    "#3a4d53",
	"yellow":  "#ad8900",
	"orange":  "#c25d1e",
	"red":     "#d2212d",
	"magenta": "#ca4898",
	"violet":  "#8762c6",
	"blue":    "#0072d4",
	"cyan":    "#009c8f",
	"green":   "#489100",
}

// accentColor returns the color of labels in rofi rows: cyan of the
// theme in use. Pango markup cannot use the colors of the rofi theme.
func accentColor() string { return paletteColor("cyan") }

// visibleColor returns the color of workspaces on other screens in
// rofi rows: blue of the theme in use.
func visibleColor() string { return paletteColor("blue") }

// selectedColor returns the color of selected items in rofi rows: magenta of the
// theme in use.
func selectedColor() string { return paletteColor("magenta") }

// paletteColor returns the color name of the theme in use, from the
// palette that dotfiles writes to ~/.config/theme/palette.json, or
// from fallbackPalette (Selenized light) if the palette cannot be read.
func paletteColor(name string) string {
	if c := palette()[name]; c != "" {
		return c
	}
	return fallbackPalette[name]
}

// palette returns the colors of ~/.config/theme/palette.json, or nil.
var palette = sync.OnceValue(func() map[string]string {
	file, err := configFile("theme", "palette.json")
	if err != nil {
		return nil
	}
	data, err := os.ReadFile(file)
	if err != nil {
		return nil
	}
	var p struct {
		Colors map[string]string `json:"colors"`
	}
	if err := json.Unmarshal(data, &p); err != nil {
		return nil
	}
	return p.Colors
})
