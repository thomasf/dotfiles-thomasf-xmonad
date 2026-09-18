//go:build linux

package main

import (
	"testing"
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

	// Screen 1: simple top template (named workspaces only)
	s1 := cfg.ConfigForScreen(1)
	if !s1.hasTop() {
		t.Errorf("expected screen 1 hasTop to be true, got %v", s1.hasTop())
	}
	if s1.topTemplate() != xmobarTopSimpleTemplate {
		t.Errorf("expected screen 1 to use simple top template")
	}
	if s1.Bottom {
		t.Errorf("expected screen 1 Bottom to be false, got %v", s1.Bottom)
	}

	// Screen 2: disabled
	s2 := cfg.ConfigForScreen(2)
	if s2.hasTop() || s2.Bottom || s2.Trayer {
		t.Errorf("expected screen 2 to be empty, got %+v", s2)
	}

	// Screen 3: simple top template (named workspaces only)
	s3 := cfg.ConfigForScreen(3)
	if !s3.hasTop() {
		t.Errorf("expected screen 3 hasTop to be true, got %v", s3.hasTop())
	}
	if s3.topTemplate() != xmobarTopSimpleTemplate {
		t.Errorf("expected screen 3 to use simple top template")
	}
	if s3.Bottom {
		t.Errorf("expected screen 3 Bottom to be false, got %v", s3.Bottom)
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
		s1 := cfg.ConfigForScreen(1)
		if !s1.hasTop() || s1.topTemplate() != xmobarTopSimpleTemplate {
			t.Errorf("expected normalized hostname %q to match flam config, got %+v", c, s1)
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
