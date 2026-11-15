//go:build linux

package main

import (
	"flag"

	critty "github.com/go-pa/critty-ipc"
)

// commonReadFlags defines the flags shared by all read commands:
// ws read, ws rc read, and ws emacs read.
type commonReadFlags struct {
	global   bool
	visible  bool
	except   string
	lines    int
	full     bool
	ansi     bool
	noTrim   bool
	noRedact bool
}

func (c *commonReadFlags) Register(fs *flag.FlagSet) {
	fs.BoolVar(&c.global, "global", false, "read sources across all workspaces")
	fs.BoolVar(&c.visible, "visible", false, "read sources on visible workspaces")
	fs.StringVar(&c.except, "except", "", "leave out sources on the workspace of `target`")
	fs.IntVar(&c.lines, "lines", 0, "print the last `n` rows of the history or buffer")
	fs.BoolVar(&c.full, "full", false, "print full content (entire buffer or terminal scrollback)")
	fs.BoolVar(&c.ansi, "ansi", false, "keep colors and text attributes as ANSI escape sequences")
	fs.BoolVar(&c.noTrim, "no-trim", false, "print screens without trimming whitespace")
	fs.BoolVar(&c.noRedact, "no-redact", false, "do not redact secrets via delemark")
}

func (c *commonReadFlags) validate(fs *flag.FlagSet, defaultVisible bool) error {
	if c.global && c.visible || (c.global || c.visible) && fs.NArg() > 0 {
		usageError(fs)
	}
	if c.full && c.lines != 0 || c.lines < 0 {
		usageError(fs)
	}
	if c.except != "" && !c.global && !c.visible && fs.NArg() == 0 {
		usageError(fs)
	}
	if defaultVisible && !c.global && fs.NArg() == 0 {
		c.visible = true
	}
	return nil
}

func (c *commonReadFlags) screenReader() screenReader {
	opts := screenReader{
		ReadOptions: critty.ReadOptions{Lines: c.lines, ANSI: c.ansi},
		trim:        !c.noTrim,
	}
	if c.full {
		opts.Lines = -1
	}
	return opts
}

func (c *commonReadFlags) resolveRcSkip() (string, error) {
	if c.except == "" {
		return "", nil
	}
	target, err := resolveRcTarget(c.except)
	if err != nil {
		return "", err
	}
	base, _ := splitRcName(target)
	return base, nil
}

func (c *commonReadFlags) resolveEmacsSkip() (string, error) {
	if c.except == "" {
		return "", nil
	}
	return resolveEmacsTarget(c.except)
}

// emacsReadFlags defines Emacs buffer and active selection reading options.
type emacsReadFlags struct {
	region   bool
	noHeader bool
}

func (e *emacsReadFlags) Register(fs *flag.FlagSet) {
	fs.BoolVar(&e.region, "region", false, "print the marked active region")
	fs.BoolVar(&e.noHeader, "no-header", false, "print region without file and line header")
}

// rcReadFlags defines options specific to remote control terminals.
type rcReadFlags struct {
	windows bool
}

func (r *rcReadFlags) Register(fs *flag.FlagSet) {
	fs.BoolVar(&r.windows, "windows", false, "read every terminal of each target")
}

// sourceFlags defines source selection options for ws read.
type sourceFlags struct {
	emacs bool
	rc    bool
}

func (s *sourceFlags) Register(fs *flag.FlagSet) {
	fs.BoolVar(&s.emacs, "emacs", false, "include Emacs buffers")
	fs.BoolVar(&s.rc, "rc", false, "include remote control terminals")
}
