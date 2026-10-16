//go:build linux

// Command kbd loads the xkb keymaps in ~/.config-xmonad/xkb into X.
//
//	kbd [-n] [load]
//	kbd [-n] set NAME|auto
//	kbd list
//	kbd [-n] pick
//	kbd [-n] watch
//
// Load gives each connected keyboard the keymap the rules below pick for
// it and turns num lock on. Set switches every keyboard to the keymap NAME
// until set auto goes back to the rules. The choice is kept in
// $XDG_STATE_HOME/kbd/keymap, so it holds for keyboards plugged in later
// and for the next login. List prints the keymaps, the selected one marked
// with *, and pick chooses one with rofi. Watch loads, then keeps running
// and loads again whenever an input device is plugged in. With -n kbd
// prints the commands instead of running them. The hundreds of warnings
// xkbcomp prints for keys X has no keycodes for are hidden unless
// -verbose is given.
package main

import (
	"bufio"
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"log"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"syscall"
	"time"
)

// rule gives the keyboards whose xinput name contains match, in any case,
// the keymap.
type rule struct {
	match  string
	keymap string
}

// The first of these that matches a connected keyboard sets the keymap of
// every keyboard.
var everyKeyboard = []rule{
	{"Kenth", "my-mac-us-ctrl-caps-super"},
	{"USBPS2", "my-us-intl"},
	{"8BitDo 8BitDo Retro Keyboard", "my-us-intl"},
	{"TrulyErgonomic.com", "my-us-intl"},
	{"DATACOMP SteelS", "my-se-caps-ctrl-super"},
	{"Apple Internal Keyboard", "my-mac-us-ctrl-caps-super"},
}

// Then each keyboard that one of these matches gets its own keymap.
var perKeyboard = []rule{
	{"Apple Internal Keyboard", "my-mac-us-ctrl-caps-super"},
	{"AT Translated Set 2 keyboard", "my-us-ctrl-caps"},
}

// match returns the first of rules that matches one of devs.
func match(rules []rule, devs ...device) (rule, bool) {
	for _, r := range rules {
		for _, d := range devs {
			if strings.Contains(strings.ToLower(d.name), strings.ToLower(r.match)) {
				return r, true
			}
		}
	}
	return rule{}, false
}

type device struct {
	id   string
	name string
}

var slaveKeyboardRe = regexp.MustCompile(`↳\s*(.*?)\s+id=(\d+)\s+\[slave\s+keyboard`)

// parseKeyboards returns the slave keyboards in the output of `xinput list`.
func parseKeyboards(out string) []device {
	var devs []device
	for line := range strings.Lines(out) {
		if m := slaveKeyboardRe.FindStringSubmatch(line); m != nil {
			devs = append(devs, device{id: m[2], name: m[1]})
		}
	}
	return devs
}

// auto is the keymap name that means loading keymaps by the rules.
const auto = "auto"

// commands returns the commands that load keymap, or the keymaps by the
// rules if it is auto, for the connected keyboards devs, then turn num lock
// on if numlock is set, since loading a keymap can turn it off. The rules
// start from the X session's default keymap, so that keyboards no rule
// matches don't keep a keymap switched to earlier. Unless verbose is set,
// xkbcomp is told not to print warnings.
func commands(devs []device, xkbDir, display, keymap string, numlock, verbose bool) [][]string {
	xkbcomp := func(keymap string, deviceArgs ...string) []string {
		args := []string{"xkbcomp"}
		if !verbose {
			args = append(args, "-w", "0")
		}
		return slices.Concat(args, []string{"-I" + xkbDir}, deviceArgs,
			[]string{filepath.Join(xkbDir, keymap), display})
	}
	var cmds [][]string
	if keymap != auto {
		cmds = append(cmds, xkbcomp(keymap))
	} else {
		cmds = append(cmds, []string{"setxkbmap"})
		if r, ok := match(everyKeyboard, devs...); ok {
			cmds = append(cmds, xkbcomp(r.keymap))
		}
		for _, d := range devs {
			if r, ok := match(perKeyboard, d); ok {
				cmds = append(cmds, xkbcomp(r.keymap, "-i", d.id))
			}
		}
	}
	if numlock {
		cmds = append(cmds, []string{"numlockx", "on"})
	}
	return cmds
}

// kbd is where the keymaps and the selected keymap are and how kbd reaches
// X. Tests replace xinput and run.
type kbd struct {
	xkbDir    string
	stateFile string
	display   string
	dryRun    bool
	verbose   bool // pass on xkbcomp's warnings
	numlockx  bool // numlockx is installed

	xinput func() (string, error)    // returns the output of `xinput list`
	run    func(args []string) error // runs, or with -n prints, a command
}

func newKbd(dryRun, verbose bool) (kbd, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return kbd{}, err
	}
	stateDir := os.Getenv("XDG_STATE_HOME")
	if stateDir == "" {
		stateDir = filepath.Join(home, ".local", "state")
	}
	_, err = exec.LookPath("numlockx")
	k := kbd{
		xkbDir:    filepath.Join(home, ".config-xmonad", "xkb"),
		stateFile: filepath.Join(stateDir, "kbd", "keymap"),
		display:   os.Getenv("DISPLAY"),
		dryRun:    dryRun,
		verbose:   verbose,
		numlockx:  err == nil,
		xinput:    xinputList,
		run:       runCommand,
	}
	if dryRun {
		k.run = printCommand
	}
	return k, nil
}

func xinputList() (string, error) {
	out, err := exec.Command("xinput", "list").Output()
	return string(out), err
}

func runCommand(args []string) error {
	cmd := exec.Command(args[0], args[1:]...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func printCommand(args []string) error {
	fmt.Println(strings.Join(args, " "))
	return nil
}

// load loads keymap, or the keymaps by the rules if it is auto.
func (k kbd) load(keymap string) error {
	out, err := k.xinput()
	if err != nil {
		return fmt.Errorf("xinput list: %w", err)
	}
	var errs []error
	for _, args := range commands(parseKeyboards(out), k.xkbDir, k.display, keymap, k.numlockx, k.verbose) {
		if err := k.run(args); err != nil {
			errs = append(errs, fmt.Errorf("%s: %w", strings.Join(args, " "), err))
		}
	}
	return errors.Join(errs...)
}

// keymaps returns the names of the keymaps in the xkb directory.
func (k kbd) keymaps() ([]string, error) {
	entries, err := os.ReadDir(k.xkbDir)
	if err != nil {
		return nil, err
	}
	var names []string
	for _, e := range entries {
		if !e.IsDir() && !strings.HasPrefix(e.Name(), ".") {
			names = append(names, e.Name())
		}
	}
	return names, nil
}

// selected returns the keymap chosen with set, or auto.
func (k kbd) selected() string {
	b, err := os.ReadFile(k.stateFile)
	if err != nil {
		return auto
	}
	name := strings.TrimSpace(string(b))
	if _, err := os.Stat(filepath.Join(k.xkbDir, name)); name == "" || err != nil {
		return auto
	}
	return name
}

// set makes name, a keymap or auto, the selected keymap and loads it.
func (k kbd) set(name string) error {
	if err := k.check(name); err != nil {
		return err
	}
	if !k.dryRun {
		if err := k.save(name); err != nil {
			return err
		}
	}
	return k.load(name)
}

// check returns an error unless name is auto or a keymap.
func (k kbd) check(name string) error {
	if name == auto {
		return nil
	}
	names, err := k.keymaps()
	if err != nil {
		return err
	}
	if !slices.Contains(names, name) {
		return fmt.Errorf("no keymap %q, see kbd list", name)
	}
	return nil
}

// save makes name, a keymap or auto, the selected keymap.
func (k kbd) save(name string) error {
	if name == auto {
		if err := os.Remove(k.stateFile); err != nil && !errors.Is(err, fs.ErrNotExist) {
			return err
		}
		return nil
	}
	if err := os.MkdirAll(filepath.Dir(k.stateFile), 0o755); err != nil {
		return err
	}
	return os.WriteFile(k.stateFile, []byte(name+"\n"), 0o644)
}

// choices returns auto and the keymaps, and the index of the selected one.
func (k kbd) choices() ([]string, int, error) {
	names, err := k.keymaps()
	if err != nil {
		return nil, 0, err
	}
	names = append([]string{auto}, names...)
	return names, slices.Index(names, k.selected()), nil
}

// list writes auto and the keymaps to w, the selected one marked with *.
func (k kbd) list(w io.Writer) error {
	names, current, err := k.choices()
	if err != nil {
		return err
	}
	for i, name := range names {
		mark := " "
		if i == current {
			mark = "*"
		}
		fmt.Fprintln(w, mark, name)
	}
	return nil
}

// pick lets the user choose a keymap with rofi and switches to it.
func (k kbd) pick() error {
	names, current, err := k.choices()
	if err != nil {
		return err
	}
	cmd := exec.Command("rofi", "-dmenu", "-i", "-no-custom", "-p", "kbd",
		"-selected-row", strconv.Itoa(current))
	cmd.Stdin = strings.NewReader(strings.Join(names, "\n"))
	cmd.Stderr = os.Stderr
	out, err := cmd.Output()
	if err != nil {
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) {
			return nil // cancelled
		}
		return fmt.Errorf("rofi: %w", err)
	}
	return k.set(strings.TrimSpace(string(out)))
}

// watch loads the selected keymap, then again after each input device is
// plugged in, until it is interrupted.
func (k kbd) watch() error {
	loadSelected := func() {
		if err := k.load(k.selected()); err != nil {
			log.Print(err)
		}
	}
	loadSelected()

	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()
	cmd := exec.CommandContext(ctx, "udevadm", "monitor", "--udev", "--subsystem-match=input")
	events, err := cmd.StdoutPipe()
	if err != nil {
		return err
	}
	if err := cmd.Start(); err != nil {
		return fmt.Errorf("udevadm monitor: %w", err)
	}
	err = watchEvents(events, time.Second, loadSelected)
	if waitErr := cmd.Wait(); err == nil && ctx.Err() == nil {
		err = fmt.Errorf("udevadm monitor exited: %v", waitErr)
	}
	return err
}

// watchEvents calls load after each burst of input device add events in
// the output of `udevadm monitor`, once no new event has come in for
// debounce. It returns when events is exhausted.
func watchEvents(events io.Reader, debounce time.Duration, load func()) error {
	added := make(chan struct{})
	scanErr := make(chan error, 1)
	go func() {
		sc := bufio.NewScanner(events)
		for sc.Scan() {
			// UDEV  [12345.678901] add      /devices/.../input/input25 (input)
			if f := strings.Fields(sc.Text()); len(f) > 2 && f[0] == "UDEV" && f[2] == "add" {
				added <- struct{}{}
			}
		}
		scanErr <- sc.Err()
	}()

	timer := time.NewTimer(debounce)
	timer.Stop()
	for {
		select {
		case <-added:
			timer.Reset(debounce)
		case <-timer.C:
			load()
		case err := <-scanErr:
			return err
		}
	}
}

// errUsage makes kbd print its usage and exit with status 2.
var errUsage = errors.New("usage")

// command runs the kbd command in args.
func (k kbd) command(args []string) error {
	cmd := "load"
	if len(args) > 0 {
		cmd, args = args[0], args[1:]
	}
	switch {
	case cmd == "load" && len(args) == 0:
		return k.load(k.selected())
	case cmd == "set" && len(args) == 1:
		return k.set(args[0])
	case cmd == "list" && len(args) == 0:
		return k.list(os.Stdout)
	case cmd == "pick" && len(args) == 0:
		return k.pick()
	case cmd == "watch" && len(args) == 0:
		return k.watch()
	}
	return errUsage
}

func usage() {
	fmt.Fprint(flag.CommandLine.Output(), `usage:
  kbd [-n] [load]    load the selected keymap, or the keymaps by the rules
  kbd [-n] set NAME  switch every keyboard to the keymap NAME
  kbd [-n] set auto  go back to the keymaps by the rules
  kbd list           list the keymaps, the selected one marked with *
  kbd [-n] pick      choose the keymap with rofi
  kbd [-n] watch     load, and again whenever an input device is plugged in

`)
	flag.PrintDefaults()
}

func main() {
	log.SetFlags(0)
	log.SetPrefix("kbd: ")
	flag.Usage = usage
	dryRun := flag.Bool("n", false, "print the commands instead of running them")
	verbose := flag.Bool("verbose", false, "show xkbcomp's warnings")
	flag.Parse()

	k, err := newKbd(*dryRun, *verbose)
	if err == nil {
		err = k.command(flag.Args())
	}
	if errors.Is(err, errUsage) {
		flag.Usage()
		os.Exit(2)
	}
	if err != nil {
		log.Fatal(err)
	}
}
