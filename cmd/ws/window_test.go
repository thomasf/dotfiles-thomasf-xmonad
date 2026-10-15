//go:build linux

package main

import (
	"slices"
	"testing"

	"github.com/BurntSushi/xgb/xproto"
)

func TestWindowRows(t *testing.T) {
	got := windowRows([]window{{1, "a <b>\nc", "Emacs", "x&y"}})
	want := []string{"a &lt;b&gt; c<span font_family='Consolas'> — Emacs</span> ❨x&amp;y❩"}
	if !slices.Equal(got, want) {
		t.Errorf("windowRows = %q, want %q", got, want)
	}
}

func TestTextProperty(t *testing.T) {
	latin1 := &xproto.GetPropertyReply{Type: xproto.AtomString, Value: []byte("caf\xe9")}
	if got := textProperty(latin1); got != "café" {
		t.Errorf("textProperty(STRING) = %q, want %q", got, "café")
	}
}
