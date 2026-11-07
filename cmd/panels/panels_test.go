//go:build linux

package main

import (
	"strings"
	"testing"
	"text/template"
)

func TestComputerConfig_Flam(t *testing.T) {
	cfg := getComputerConfig("flam")

	// Screen 0: top (standard), bottom, trayer
	s0 := cfg.ConfigForScreen(0)
	if !s0.hasTop() {
		t.Errorf("expected screen 0 hasTop to be true, got %v", s0.hasTop())
	}
	if s0.topTemplate() != xmobarTopTemplate {
		t.Errorf("expected screen 0 to use standard top template")
	}
	if !s0.Bottom {
		t.Errorf("expected screen 0 Bottom to be true, got %v", s0.Bottom)
	}
	if !s0.Trayer {
		t.Errorf("expected screen 0 Trayer to be true, got %v", s0.Trayer)
	}

	// Other screens: no panels
	for _, screen := range []int{1, 2, 3} {
		s := cfg.ConfigForScreen(screen)
		if s.hasTop() || s.Bottom || s.Trayer {
			t.Errorf("expected screen %d to be empty, got %+v", screen, s)
		}
	}
}

func TestComputerConfig_Transwhale(t *testing.T) {
	cfg := getComputerConfig("transwhale")

	// Screen 0: top, bottom, trayer
	s0 := cfg.ConfigForScreen(0)
	if !s0.hasTop() || !s0.Bottom || !s0.Trayer {
		t.Errorf("expected screen 0 Top, Bottom, Trayer to be true, got %+v", s0)
	}

	// Other screens: no bars
	s1 := cfg.ConfigForScreen(1)
	if s1.hasTop() || s1.Bottom || s1.Trayer {
		t.Errorf("expected screen 1 to have no panels, got %+v", s1)
	}
}

func TestComputerConfig_Default(t *testing.T) {
	cfg := getComputerConfig("unknown-host")

	// Screen 0: top, bottom, trayer
	s0 := cfg.ConfigForScreen(0)
	if !s0.hasTop() || !s0.Bottom || !s0.Trayer {
		t.Errorf("expected screen 0 Top, Bottom, Trayer to be true, got %+v", s0)
	}

	// Other screens: top and bottom, no trayer
	s1 := cfg.ConfigForScreen(1)
	if !s1.hasTop() || !s1.Bottom || s1.Trayer {
		t.Errorf("expected screen 1 Top and Bottom to be true, Trayer to be false, got %+v", s1)
	}
	if s1.topTemplate() != xmobarTopTemplate {
		t.Errorf("expected screen 1 default to use standard top template")
	}
}

func TestGetComputerConfig_HostnameNormalization(t *testing.T) {
	cases := []string{"FLAM", "flam.local", "flam.domain.com", " flam "}
	for _, c := range cases {
		cfg := getComputerConfig(c)
		s0 := cfg.ConfigForScreen(0)
		if !s0.hasTop() || !s0.Bottom || !s0.Trayer || s0.topTemplate() != xmobarTopTemplate {
			t.Errorf("expected normalized hostname %q to match flam config for screen 0, got %+v", c, s0)
		}
		s1 := cfg.ConfigForScreen(1)
		if s1.hasTop() || s1.Bottom || s1.Trayer {
			t.Errorf("expected normalized hostname %q to have no panels on screen 1, got %+v", c, s1)
		}
	}
}

func TestComputerConfig_NilScreens(t *testing.T) {
	cfg := ComputerConfig{
		Default: ScreenConfig{Top: true},
	}
	s := cfg.ConfigForScreen(0)
	if !s.hasTop() || s.Bottom || s.Trayer {
		t.Errorf("expected fallback to default when Screens is nil, got %+v", s)
	}
}

func TestRenderContext_Scale(t *testing.T) {
	// Baseline scale (96 DPI)
	rc1 := RenderContext{DpiScale: 1.0}
	if got := rc1.Scale(18.62); got != 19 {
		t.Errorf("Scale(18.62) at 1.0 scale: expected 19, got %d", got)
	}
	if got := rc1.Scale(15.7); got != 16 {
		t.Errorf("Scale(15.7) at 1.0 scale: expected 16, got %d", got)
	}

	// Flam scale (165 DPI: 165 / 96 = 1.71875)
	rcFlam := RenderContext{DpiScale: 1.71875}
	if got := rcFlam.Scale(18.62); got != 32 {
		t.Errorf("Scale(18.62) at flam scale: expected 32, got %d", got)
	}
	if got := rcFlam.Scale(15.7); got != 27 {
		t.Errorf("Scale(15.7) at flam scale: expected 27, got %d", got)
	}
	if got := rcFlam.Scale(9.89); got != 17 {
		t.Errorf("Scale(9.89) at flam scale: expected 17, got %d", got)
	}
	if got := rcFlam.Scale(9.31); got != 16 {
		t.Errorf("Scale(9.31) at flam scale: expected 16, got %d", got)
	}
	if got := rcFlam.Scale(15.13); got != 26 {
		t.Errorf("Scale(15.13) at flam scale: expected 26, got %d", got)
	}
	if got := rcFlam.Scale(10.47); got != 18 {
		t.Errorf("Scale(10.47) at flam scale: expected 18, got %d", got)
	}
}

func TestTemplate_Rendering(t *testing.T) {
	rc := RenderContext{
		Colors: map[string]string{
			"bg_0": "#fbf3db",
			"bg_1": "#ece3cc",
			"fg_0": "#53676d",
			"fg_1": "#3a4d53",
		},
		Height:   32,
		DpiScale: 1.71875,
	}

	for name, tmpl := range map[string]string{
		"top":        xmobarTopTemplate,
		"top_simple": xmobarTopSimpleTemplate,
		"bottom":     xmobarBottomTemplate,
	} {
		tpl, err := template.New(name).Parse(tmpl)
		if err != nil {
			t.Fatalf("failed to parse template %s: %v", name, err)
		}
		var buf strings.Builder
		if err := tpl.Execute(&buf, rc); err != nil {
			t.Fatalf("failed to execute template %s: %v", name, err)
		}
		out := buf.String()
		if name == "bottom" {
			if !strings.Contains(out, "Roboto Condensed 26px") {
				t.Errorf("expected bottom bar to contain Roboto Condensed 26px, got:\n%s", out)
			}
			if !strings.Contains(out, "BottomSize C 100 32") {
				t.Errorf("expected bottom bar to contain BottomSize C 100 32, got:\n%s", out)
			}
		} else {
			if !strings.Contains(out, "Inter Medium 27px") {
				t.Errorf("expected %s to contain Inter Medium 27px, got:\n%s", name, out)
			}
		}
	}
}
