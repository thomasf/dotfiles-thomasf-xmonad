//go:build linux

package main

import (
	"flag"
	"strings"
	"testing"
)

func TestCommonReadFlags(t *testing.T) {
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	var common commonReadFlags
	common.Register(fs)

	if err := fs.Parse([]string{"-global", "-no-redact", "-lines", "50", "-ansi"}); err != nil {
		t.Fatal(err)
	}

	if !common.global {
		t.Errorf("common.global = false, want true")
	}
	if !common.noRedact {
		t.Errorf("common.noRedact = false, want true")
	}
	if common.visible {
		t.Errorf("common.visible = true, want false")
	}
	if common.lines != 50 {
		t.Errorf("common.lines = %d, want 50", common.lines)
	}
	if !common.ansi {
		t.Errorf("common.ansi = false, want true")
	}

	if err := common.validate(fs, false); err != nil {
		t.Fatalf("common.validate() error: %v", err)
	}

	reader := common.screenReader()
	if reader.Lines != 50 || !reader.ANSI || !reader.trim {
		t.Errorf("reader = %+v, want lines=50 ansi=true trim=true", reader)
	}
}

func TestCommonReadFlagsFull(t *testing.T) {
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	var common commonReadFlags
	common.Register(fs)

	if err := fs.Parse([]string{"-full"}); err != nil {
		t.Fatal(err)
	}

	if !common.full {
		t.Errorf("common.full = false, want true")
	}

	reader := common.screenReader()
	if reader.Lines != -1 {
		t.Errorf("reader.Lines = %d, want -1", reader.Lines)
	}
}

func TestEmacsReadFlags(t *testing.T) {
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	var emacs emacsReadFlags
	emacs.Register(fs)

	if err := fs.Parse([]string{"-region", "-no-header"}); err != nil {
		t.Fatal(err)
	}

	if !emacs.region {
		t.Errorf("emacs.region = false, want true")
	}
	if !emacs.noHeader {
		t.Errorf("emacs.noHeader = false, want true")
	}
}

func TestRcReadFlags(t *testing.T) {
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	var rc rcReadFlags
	rc.Register(fs)

	if err := fs.Parse([]string{"-windows"}); err != nil {
		t.Fatal(err)
	}

	if !rc.windows {
		t.Errorf("rc.windows = false, want true")
	}
}

func TestCommandUsage(t *testing.T) {
	emacsCmd := newEmacsCommand()
	usage := commandUsage(emacsCmd)
	if !strings.Contains(usage, "usage: ws emacs <command> [arguments]") {
		t.Errorf("commandUsage(emacs) missing ShortUsage: %s", usage)
	}
	if !strings.Contains(usage, "Commands:") || !strings.Contains(usage, "list") || !strings.Contains(usage, "read") {
		t.Errorf("commandUsage(emacs) missing subcommands: %s", usage)
	}

	readCmd := newEmacsReadCommand()
	readUsage := commandUsage(readCmd)
	if !strings.Contains(readUsage, "Flags:") || !strings.Contains(readUsage, "-region") || !strings.Contains(readUsage, "-full") {
		t.Errorf("commandUsage(emacs read) missing expected flags: %s", readUsage)
	}

	rcCmd := newRcCommand()
	rcUsage := commandUsage(rcCmd)
	if !strings.Contains(rcUsage, "usage: ws rc <command> [arguments]") {
		t.Errorf("commandUsage(rc) missing ShortUsage: %s", rcUsage)
	}
	if !strings.Contains(rcUsage, "list") || !strings.Contains(rcUsage, "send") || !strings.Contains(rcUsage, "read") {
		t.Errorf("commandUsage(rc) missing subcommands: %s", rcUsage)
	}
}
