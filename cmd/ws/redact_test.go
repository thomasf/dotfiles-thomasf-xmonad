//go:build linux

package main

import (
	"os/exec"
	"testing"
)

func TestRedact(t *testing.T) {
	if got := redact(""); got != "" {
		t.Fatalf("redact(\"\") = %q, want empty", got)
	}

	clean := "clean text without secrets\n"
	if got := redact(clean); got != clean {
		t.Fatalf("redact(clean) = %q, want %q", got, clean)
	}

	if _, err := exec.LookPath("delemark"); err == nil {
		secretText := "api key divio_pak_dGVzdC1zZWNyZXQta2V5LXRva2VuLWZvci1kaXZpby0 here\n"
		got := redact(secretText)
		want1 := "api key REDACTED here\n"
		want2 := "api key [REDACTED] here\n"
		if got != want1 && got != want2 {
			t.Fatalf("redact(secretText) = %q, want %q or %q", got, want1, want2)
		}
	}
}
