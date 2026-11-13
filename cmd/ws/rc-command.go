//go:build linux

package main

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"text/tabwriter"
	"time"
	"unicode"

	critty "github.com/go-pa/critty-ipc"
)

const rcUsage = `usage: ws rc list [-global | -visible | target]
       ws rc send [-windows] [-paste] [-cr] target [text...]
       ws rc read [-ansi] [-no-trim] [-lines n | -history] [-global | -visible | [-windows] target...]

Rc works with the critty terminals that ws term started with remote
control. Their names are the file names of their workspaces (see ws
name), with +2, +3 and so on for further terminals on a workspace.

A target is relative to the workspace of the terminal ws runs in, from
$WS_WORKSPACE, which ws term sets, or else to the current workspace.
Here it is dpg@x.bot:

    .logs      the terminals of dpg@x.logs
    .logs+2    the second terminal of dpg@x.logs
    +2         the second terminal of dpg@x.bot
    dpg@y.test the terminals of another workspace, by its full name

send and read address the primary terminal of a target, the one with
the lowest number. With -global, list and read show every remote
control terminal on the display, and with -visible every terminal on
the workspaces shown on the screens.

Run ws rc <command> -h for help on a command.
`

func runRc(args []string) error {
	if len(args) == 0 {
		fmt.Fprint(os.Stderr, rcUsage)
		os.Exit(2)
	}
	switch cmd, args := args[0], args[1:]; cmd {
	case "list", "ls":
		return runRcList(args)
	case "send":
		return runRcSend(args)
	case "read":
		return runRcRead(args)
	case "-h", "-help", "--help", "help":
		fmt.Fprint(os.Stdout, rcUsage)
		os.Exit(0)
	}
	fmt.Fprint(os.Stderr, rcUsage)
	os.Exit(2)
	return nil
}

func runRcList(args []string) error {
	fs := newFlagSet("rc list", "[-global | -visible | target]", `
List prints the remote control terminals of a target, by default those
of every workspace with the prefix of the current one. A target without
a suffix, such as dpg or dpg@x, lists every workspace it is the prefix
or project of. -global lists every remote control terminal on the
display, including those not started by ws term, and -visible those on
the workspaces shown on the screens, the current workspace first.

The columns are the remote control name, the workspace, the process ID
of the terminal and the working directory and command of the program in
the foreground of its shell.
`)
	global := fs.Bool("global", false, "list every remote control terminal on the display")
	visible := fs.Bool("visible", false, "list the remote control terminals on the visible workspaces")
	fs.Parse(args)
	checkArgs(fs, 0, 1)
	if *global && *visible || (*global || *visible) && fs.NArg() > 0 {
		usageError(fs)
	}
	terms, err := rcTerminals()
	if err != nil {
		return err
	}
	switch {
	case *visible:
		wins, err := visibleTerminalWindows()
		if err != nil {
			return err
		}
		var shown []rcTerminal
		for _, w := range wins {
			if i := slices.IndexFunc(terms, func(t rcTerminal) bool { return t.pid == w.pid }); i >= 0 && !slices.Contains(shown, terms[i]) {
				shown = append(shown, terms[i])
			}
		}
		terms = shown
	case !*global:
		target := fs.Arg(0)
		if target == "" {
			w, err := contextWorkspace()
			if err != nil {
				return err
			}
			target = fileName(w.prefix())
		} else if target, err = resolveRcTarget(target); err != nil {
			return err
		}
		terms = slices.DeleteFunc(terms, func(t rcTerminal) bool { return !t.inList(target) })
		if len(terms) == 0 {
			return fmt.Errorf("no remote control terminal for %s", target)
		}
	}
	names, _, _ := desktops()
	tree := readProcTree()
	tw := tabwriter.NewWriter(os.Stdout, 0, 4, 2, ' ', 0)
	fmt.Fprintln(tw, "NAME\tWORKSPACE\tPID\tCWD\tCOMMAND")
	for _, t := range terms {
		ws := ""
		if t.base != "" {
			if i := slices.IndexFunc(names, func(n string) bool { return fileName(n) == t.base }); i >= 0 {
				ws = names[i]
			}
		}
		pid, cwd, cmd := "", "", ""
		if t.pid > 0 {
			pid = strconv.Itoa(t.pid)
			cwd, cmd = foreground(tree, t.pid)
			cwd = shortenHome(cwd)
		}
		fmt.Fprintf(tw, "%s\t%s\t%s\t%s\t%s\n", t.name, ws, pid, cwd, cmd)
	}
	return tw.Flush()
}

func runRcSend(args []string) error {
	fs := newFlagSet("rc send", "[-windows] [-paste] [-cr] target [text...]", `
Send types text, the arguments joined by spaces or else standard input,
into the primary terminal of target. With -windows it types into every
terminal of the target but the one ws runs in.

-paste sends the text as a bracketed paste. A program that supports
bracketed paste, such as Claude Code or a shell, takes it as one paste,
newlines and all, rather than as typed lines, each submitted by its
newline.

-cr sends a carriage return after the text, as pressing Enter does,
which runs it in a shell or submits it to a program. Without it the
text waits for the user to press Enter.
`)
	all := fs.Bool("windows", false, "send to every terminal of the target")
	paste := fs.Bool("paste", false, "send the text as a bracketed paste")
	cr := fs.Bool("cr", false, "send a carriage return after the text, as Enter does")
	fs.Parse(args)
	checkArgs(fs, 1, -1)
	terms, err := rcTerminals()
	if err != nil {
		return err
	}
	if terms, err = rcTargets(terms, fs.Arg(0), *all); err != nil {
		return err
	}
	if *all {
		self := ancestors(os.Getpid())
		terms = slices.DeleteFunc(terms, func(t rcTerminal) bool { return self[t.pid] })
		if len(terms) == 0 {
			return fmt.Errorf("no remote control terminal for %s but this one", fs.Arg(0))
		}
	}
	text := strings.Join(fs.Args()[1:], " ")
	if fs.NArg() == 1 {
		b, err := io.ReadAll(os.Stdin)
		if err != nil {
			return err
		}
		text = string(b)
	}
	if *paste {
		text = bracketedPaste(text)
	}
	if *cr {
		text += "\r"
	}
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	var errs []error
	for _, t := range terms {
		c := critty.Client{Socket: critty.RemoteControlSocket(t.name)}
		if err := c.SendText(ctx, 0, text); err != nil {
			errs = append(errs, fmt.Errorf("%s: %w", t.name, err))
		}
	}
	return errors.Join(errs...)
}

// bracketedPaste returns text wrapped in the escape sequences that mark a
// bracketed paste, with any end-of-paste sequence in it removed so that it
// cannot end the paste early.
func bracketedPaste(text string) string {
	return "\x1b[200~" + strings.ReplaceAll(text, "\x1b[201~", "") + "\x1b[201~"
}

func runRcRead(args []string) error {
	fs := newFlagSet("rc read", "[-ansi] [-no-trim] [-lines n | -history] [-global | -visible | [-windows] target...]", `
Read prints the screen of the primary terminal of each target, one
after the other. With -windows it prints those of every terminal of each
target, and with -global those of every remote control terminal on the
display.

With -visible it prints the screen of every terminal on the workspaces
shown on the screens, those of the current workspace first: the text
the user sees. A terminal that does not accept remote control is named,
but its screen cannot be read.

When read prints more than one screen, each follows a line naming its
terminal.

By default read prints the rows visible in the terminal. -lines n prints
the last n rows of the scrollback history and the screen instead, and
-history all of them; both end at the bottom of the screen, wherever the
terminal is scrolled to.

Read trims what it prints unless -no-trim is given: it drops the blank
lines at the top and bottom, the whitespace at the end of every line and
the whitespace that all lines start with. With -ansi it only drops the
blank lines at the top and bottom.
`)
	all := fs.Bool("windows", false, "read every terminal of each target")
	global := fs.Bool("global", false, "read every remote control terminal on the display")
	visible := fs.Bool("visible", false, "read every terminal on the visible workspaces")
	ansi := fs.Bool("ansi", false, "keep colors and text attributes as ANSI escape sequences")
	noTrim := fs.Bool("no-trim", false, "print the screens as they are, without trimming whitespace")
	lines := fs.Int("lines", 0, "print the last `n` rows of the history and the screen")
	history := fs.Bool("history", false, "print all of the history and the screen")
	fs.Parse(args)
	opts := screenReader{ReadOptions: critty.ReadOptions{Lines: *lines, ANSI: *ansi}, trim: !*noTrim}
	if *history {
		opts.Lines = -1
	}
	switch {
	case *lines < 0, *history && *lines != 0:
		usageError(fs)
	case *global && *visible, (*global || *visible) && (*all || fs.NArg() > 0):
		usageError(fs)
	case *visible:
		return readVisible(opts)
	case !*global:
		checkArgs(fs, 1, -1)
	}
	terms, err := rcTerminals()
	if err != nil {
		return err
	}
	var errs []error
	if !*global {
		var read []rcTerminal
		for _, target := range fs.Args() {
			t, err := rcTargets(terms, target, *all)
			if err != nil {
				errs = append(errs, err)
				continue
			}
			for _, t := range t {
				if !slices.ContainsFunc(read, func(r rcTerminal) bool { return r.name == t.name }) {
					read = append(read, t)
				}
			}
		}
		terms = read
	}
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	for i, t := range terms {
		screen, err := opts.read(ctx, t.name, 0)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		if len(terms) > 1 {
			if i > 0 {
				fmt.Println()
			}
			fmt.Printf("==> %s <==\n", t.name)
		}
		fmt.Print(screen)
	}
	return errors.Join(errs...)
}

// visibleTerminalWindows returns the terminal windows on the workspaces
// shown on the screens, those of the current workspace first.
func visibleTerminalWindows() ([]Window, error) {
	x, err := openX()
	if err != nil {
		return nil, err
	}
	defer x.close()
	names, cur, err := x.desktops()
	if err != nil {
		return nil, err
	}
	shown := x.visibleDesktops()
	if shown == nil {
		shown = make(map[int]bool)
	}
	shown[cur] = true
	wins, err := x.listWindows(names, nil)
	if err != nil {
		return nil, err
	}
	wins = slices.DeleteFunc(wins, func(w Window) bool { return !shown[w.desktopIndex] || !w.isTerminal() })
	slices.SortStableFunc(wins, func(a, b Window) int {
		return cmp.Or(cmp.Compare(rank(a.desktopIndex, cur), rank(b.desktopIndex, cur)), cmp.Compare(a.id, b.id))
	})
	return wins, nil
}

// readVisible prints the screens for ws rc read -visible, read by r.
func readVisible(r screenReader) error {
	wins, err := visibleTerminalWindows()
	if err != nil {
		return err
	}
	terms, err := rcTerminals()
	if err != nil {
		return err
	}
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	var errs []error
	for i, w := range wins {
		if i > 0 {
			fmt.Println()
		}
		j := slices.IndexFunc(terms, func(t rcTerminal) bool { return t.pid == w.pid })
		if j < 0 {
			fmt.Printf("==> %s on %s: no remote control <==\n", w.class, w.workspace)
			continue
		}
		fmt.Printf("==> %s on %s <==\n", terms[j].name, w.workspace)
		screen, err := r.read(ctx, terms[j].name, critty.Window(w.id))
		if err != nil {
			errs = append(errs, err)
			continue
		}
		fmt.Print(screen)
	}
	return errors.Join(errs...)
}

// A screenReader reads the text of terminals for ws rc read.
type screenReader struct {
	critty.ReadOptions
	trim bool // trim the text with trimScreen
}

// read returns the text of window w of the terminal with remote control
// name, the window with the lowest ID if w is 0.
func (r screenReader) read(ctx context.Context, name string, w critty.Window) (string, error) {
	c := critty.Client{Socket: critty.RemoteControlSocket(name)}
	text, err := c.ReadText(ctx, w, r.ReadOptions)
	if err != nil {
		return "", fmt.Errorf("%s: %w", name, err)
	}
	if r.trim {
		text = trimScreen(text, r.ANSI)
	}
	return text, nil
}

// ansiEscape matches the CSI escape sequences of an ANSI screen.
var ansiEscape = regexp.MustCompile(`\x1b\[[0-9;:]*[A-Za-z]`)

// trimScreen drops the blank lines at the top and bottom of screen. Unless
// ansi is set, it also drops the whitespace at the end of every line and
// the whitespace that all non-blank lines start with. A screen with any
// text left ends in a newline; one with none becomes "".
func trimScreen(screen string, ansi bool) string {
	lines := strings.Split(screen, "\n")
	blank := func(l string) bool {
		if ansi {
			l = ansiEscape.ReplaceAllString(l, "")
		}
		return strings.TrimSpace(l) == ""
	}
	for len(lines) > 0 && blank(lines[0]) {
		lines = lines[1:]
	}
	for len(lines) > 0 && blank(lines[len(lines)-1]) {
		lines = lines[:len(lines)-1]
	}
	if len(lines) == 0 {
		return ""
	}
	if !ansi {
		indent := -1
		for i, l := range lines {
			l = strings.TrimRightFunc(l, unicode.IsSpace)
			lines[i] = l
			if l == "" {
				continue
			}
			n := len(l) - len(strings.TrimLeft(l, " \t"))
			if indent < 0 || n < indent {
				indent = n
			}
		}
		for i, l := range lines {
			if len(l) >= indent {
				lines[i] = l[indent:]
			}
		}
	}
	return strings.Join(lines, "\n") + "\n"
}

// rank orders desktop i for visibleTerminalWindows: the current desktop cur
// first, then the others by index.
func rank(i, cur int) int {
	if i == cur {
		return -1
	}
	return i
}

// An rcTerminal is a running critty that accepts remote control.
type rcTerminal struct {
	name string // remote control name
	base string // file name of its workspace, from name
	n    int    // its number on the workspace: 1, then 2, 3...
	pid  int    // process ID, or 0 if unknown
}

// rcTerminals returns the running remote control terminals, sorted by
// workspace and number.
func rcTerminals() ([]rcTerminal, error) {
	names, err := critty.RemoteControlNames()
	if err != nil {
		return nil, err
	}
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	terms := make([]rcTerminal, 0, len(names))
	for _, name := range names {
		base, n := splitRcName(name)
		c := critty.Client{Socket: critty.RemoteControlSocket(name)}
		pid, _ := c.PID(ctx)
		terms = append(terms, rcTerminal{name: name, base: base, n: n, pid: pid})
	}
	slices.SortFunc(terms, func(a, b rcTerminal) int {
		return cmp.Or(strings.Compare(a.base, b.base), cmp.Compare(a.n, b.n))
	})
	return terms, nil
}

// splitRcName splits a remote control name into the file name of its
// workspace and its number on it, 1 for a name without +N.
func splitRcName(name string) (base string, n int) {
	if i := strings.LastIndexByte(name, '+'); i > 0 {
		if n, err := strconv.Atoi(name[i+1:]); err == nil && n >= 2 && name[i+1] != '0' {
			return name[:i], n
		}
	}
	return name, 1
}

// resolveRcTarget returns target as a file name, a workspace name with
// an optional +N, resolving one that starts with "." or "+" against the
// current workspace.
func resolveRcTarget(target string) (string, error) {
	if !strings.HasPrefix(target, ".") && !strings.HasPrefix(target, "+") {
		return fileName(target), nil
	}
	w, err := contextWorkspace()
	if err != nil {
		return "", err
	}
	if strings.HasPrefix(target, ".") {
		return fileName(w.prefix() + target), nil
	}
	return w.fileName() + target, nil
}

// is reports whether t is the terminal target names: the exact terminal
// for a target with +N, else any terminal of the workspace.
func (t rcTerminal) is(target string) bool {
	if base, n := splitRcName(target); n > 1 {
		return t.base == base && t.n == n
	}
	return t.base == target
}

// inList reports whether ws rc list target shows t: if t is target, or
// target is the prefix or project of t's workspace.
func (t rcTerminal) inList(target string) bool {
	return t.is(target) || strings.HasPrefix(t.base, target+".") || strings.HasPrefix(t.base, target+"@")
}

// rcTargets returns the terminals of terms that target addresses: its
// primary terminal, or with all, every terminal of it.
func rcTargets(terms []rcTerminal, target string, all bool) ([]rcTerminal, error) {
	resolved, err := resolveRcTarget(target)
	if err != nil {
		return nil, err
	}
	terms = slices.DeleteFunc(slices.Clone(terms), func(t rcTerminal) bool { return !t.is(resolved) })
	if !all && len(terms) > 1 {
		terms = terms[:1]
	}
	if len(terms) == 0 {
		return nil, fmt.Errorf("no remote control terminal for %s", resolved)
	}
	return terms, nil
}

// ancestors returns the set of pid and the processes it descends from.
func ancestors(pid int) map[int]bool {
	set := make(map[int]bool)
	for pid > 1 && !set[pid] {
		set[pid] = true
		f, ok := procStat(pid)
		if !ok {
			break
		}
		pid, _ = strconv.Atoi(f[1])
	}
	return set
}

// foreground returns the working directory and command line of the
// program in the foreground of the shell that terminal pid runs.
func foreground(tree *ProcTree, pid int) (cwd, cmd string) {
	children := tree.children[pid]
	if len(children) == 0 {
		return "", ""
	}
	fg := slices.Min(children)
	if f, ok := procStat(fg); ok {
		if tpgid, err := strconv.Atoi(f[5]); err == nil && tpgid > 0 {
			fg = tpgid
		}
	}
	cwd, _ = os.Readlink("/proc/" + strconv.Itoa(fg) + "/cwd")
	b, _ := os.ReadFile("/proc/" + strconv.Itoa(fg) + "/cmdline")
	args := strings.Fields(strings.ReplaceAll(string(b), "\x00", " "))
	if len(args) > 0 {
		args[0] = filepath.Base(args[0])
	}
	cmd = strings.Join(args, " ")
	if r := []rune(cmd); len(r) > 60 {
		cmd = string(r[:59]) + "…"
	}
	return cwd, cmd
}

// procStat returns the fields of /proc/pid/stat after the command name:
// state, ppid, pgrp, session, tty_nr, tpgid and so on.
func procStat(pid int) ([]string, bool) {
	b, err := os.ReadFile("/proc/" + strconv.Itoa(pid) + "/stat")
	if err != nil {
		return nil, false
	}
	s := string(b)
	i := strings.LastIndexByte(s, ')')
	if i < 0 {
		return nil, false
	}
	f := strings.Fields(s[i+1:])
	return f, len(f) >= 6
}
