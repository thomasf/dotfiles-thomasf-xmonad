//go:build linux

package main

import (
	"errors"
	"io"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"
)

const testXinputList = `⎡ Virtual core pointer                    	id=2	[master pointer  (3)]
⎜   ↳ Virtual core XTEST pointer              	id=4	[slave  pointer  (2)]
⎜   ↳ Apple Internal Keyboard / Trackpad      	id=6	[slave  pointer  (2)]
⎣ Virtual core keyboard                   	id=3	[master keyboard (2)]
    ↳ Virtual core XTEST keyboard             	id=5	[slave  keyboard (3)]
    ↳ AT Translated Set 2 keyboard            	id=13	[slave  keyboard (3)]
    ↳ Apple Internal Keyboard / Trackpad      	id=114	[slave  keyboard (3)]
    ↳ Apple Internal Keyboard / Trackpad      	id=115	[slave  keyboard (3)]
`

func TestParseKeyboards(t *testing.T) {
	got := parseKeyboards(testXinputList)
	want := []device{
		{"5", "Virtual core XTEST keyboard"},
		{"13", "AT Translated Set 2 keyboard"},
		{"114", "Apple Internal Keyboard / Trackpad"},
		{"115", "Apple Internal Keyboard / Trackpad"},
	}
	if !slices.Equal(got, want) {
		t.Errorf("parseKeyboards:\n got %v\nwant %v", got, want)
	}
}

func joinAll(cmds [][]string) []string {
	var lines []string
	for _, args := range cmds {
		lines = append(lines, strings.Join(args, " "))
	}
	return lines
}

func TestCommands(t *testing.T) {
	for _, tc := range []struct {
		name    string
		devs    []device
		keymap  string
		numlock bool
		verbose bool
		want    []string
	}{{
		name:   "rules",
		devs:   parseKeyboards(testXinputList),
		keymap: auto,
		want: []string{
			"setxkbmap",
			"xkbcomp -w 0 -I/x /x/my-mac-us-ctrl-caps-super :0",
			"xkbcomp -w 0 -I/x -i 13 /x/my-us-ctrl-caps :0",
			"xkbcomp -w 0 -I/x -i 114 /x/my-mac-us-ctrl-caps-super :0",
			"xkbcomp -w 0 -I/x -i 115 /x/my-mac-us-ctrl-caps-super :0",
		},
	}, {
		name:   "first every keyboard rule wins",
		devs:   []device{{"20", "DATACOMP SteelSeries"}, {"21", "TrulyErgonomic.com Keyboard"}},
		keymap: auto,
		want: []string{
			"setxkbmap",
			"xkbcomp -w 0 -I/x /x/my-us-intl :0",
		},
	}, {
		name:    "no rule matches",
		devs:    []device{{"5", "Virtual core XTEST keyboard"}},
		keymap:  auto,
		numlock: true,
		want:    []string{"setxkbmap", "numlockx on"},
	}, {
		name:    "switched",
		devs:    parseKeyboards(testXinputList),
		keymap:  "my-se-default",
		numlock: true,
		want:    []string{"xkbcomp -w 0 -I/x /x/my-se-default :0", "numlockx on"},
	}, {
		name:    "verbose",
		devs:    parseKeyboards(testXinputList),
		keymap:  "my-se-default",
		verbose: true,
		want:    []string{"xkbcomp -I/x /x/my-se-default :0"},
	}} {
		t.Run(tc.name, func(t *testing.T) {
			got := joinAll(commands(tc.devs, "/x", ":0", tc.keymap, tc.numlock, tc.verbose))
			if !slices.Equal(got, tc.want) {
				t.Errorf("commands:\n got %q\nwant %q", got, tc.want)
			}
		})
	}
}

// testKbd returns a kbd with the keymaps names in a temporary xkb
// directory, whose commands are recorded in ran instead of run.
func testKbd(t *testing.T, names ...string) (k *kbd, ran *[]string) {
	t.Helper()
	dir := t.TempDir()
	ran = new([]string)
	k = &kbd{
		xkbDir:    filepath.Join(dir, "xkb"),
		stateFile: filepath.Join(dir, "state", "kbd", "keymap"),
		display:   ":0",
		xinput:    func() (string, error) { return testXinputList, nil },
		run: func(args []string) error {
			*ran = append(*ran, strings.Join(args, " "))
			return nil
		},
	}
	if err := os.MkdirAll(filepath.Join(k.xkbDir, "symbols"), 0o755); err != nil {
		t.Fatal(err)
	}
	for _, name := range names {
		if err := os.WriteFile(filepath.Join(k.xkbDir, name), nil, 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return k, ran
}

func TestLoad(t *testing.T) {
	k, ran := testKbd(t)
	k.numlockx = true
	run := k.run
	k.run = func(args []string) error {
		run(args)
		if args[0] == "setxkbmap" {
			return errors.New("exit status 1")
		}
		return nil
	}

	err := k.load(auto)
	if err == nil || !strings.Contains(err.Error(), "setxkbmap: exit status 1") {
		t.Errorf("load error = %v, want the setxkbmap failure", err)
	}
	if n := len(*ran); n != 6 {
		t.Errorf("load ran %d commands, want all 6 despite the failure: %q", n, *ran)
	}

	k.xinput = func() (string, error) { return "", errors.New("unable to connect to X server") }
	if err := k.load(auto); err == nil {
		t.Error("load without X: no error")
	}
}

func TestSet(t *testing.T) {
	k, ran := testKbd(t, "my-us-intl", "my-se-default")
	xkbcomp := "xkbcomp -w 0 -I" + k.xkbDir + " " + filepath.Join(k.xkbDir, "my-se-default") + " :0"

	k.dryRun = true
	if err := k.set("my-se-default"); err != nil {
		t.Fatal(err)
	}
	if got := k.selected(); got != auto {
		t.Errorf("selected after set -n = %q, want auto", got)
	}
	if want := []string{xkbcomp}; !slices.Equal(*ran, want) {
		t.Errorf("set -n ran %q, want %q", *ran, want)
	}

	k.dryRun = false
	*ran = nil
	if err := k.set("my-se-default"); err != nil {
		t.Fatal(err)
	}
	if got := k.selected(); got != "my-se-default" {
		t.Errorf("selected after set = %q, want my-se-default", got)
	}
	if want := []string{xkbcomp}; !slices.Equal(*ran, want) {
		t.Errorf("set ran %q, want %q", *ran, want)
	}

	*ran = nil
	for _, name := range []string{"dvorak", "symbols", ""} {
		if err := k.set(name); err == nil {
			t.Errorf("set %q: no error", name)
		}
	}
	if len(*ran) != 0 {
		t.Errorf("failed sets ran %q", *ran)
	}
	if got := k.selected(); got != "my-se-default" {
		t.Errorf("selected after failed sets = %q, want my-se-default", got)
	}

	if err := k.set(auto); err != nil {
		t.Fatal(err)
	}
	if got := k.selected(); got != auto {
		t.Errorf("selected after set auto = %q, want auto", got)
	}
	if err := k.set(auto); err != nil {
		t.Errorf("set auto with nothing saved: %v", err)
	}
}

func TestSelected(t *testing.T) {
	k, _ := testKbd(t, "my-us-intl", "my-se-default")
	if err := k.save("my-se-default"); err != nil {
		t.Fatal(err)
	}
	var b strings.Builder
	if err := k.list(&b); err != nil {
		t.Fatal(err)
	}
	if want := "  auto\n* my-se-default\n  my-us-intl\n"; b.String() != want {
		t.Errorf("list:\n%s\nwant:\n%s", b.String(), want)
	}

	// A saved keymap that has since been removed falls back to the rules.
	if err := os.Remove(filepath.Join(k.xkbDir, "my-se-default")); err != nil {
		t.Fatal(err)
	}
	if got := k.selected(); got != auto {
		t.Errorf("selected after removing the keymap = %q, want auto", got)
	}
}

func TestCommand(t *testing.T) {
	k, ran := testKbd(t, "my-us-intl")
	for _, args := range [][]string{
		{"set"},
		{"set", "a", "b"},
		{"load", "x"},
		{"list", "x"},
		{"my-us-intl"},
	} {
		if err := k.command(args); !errors.Is(err, errUsage) {
			t.Errorf("command %q = %v, want usage", args, err)
		}
	}
	if len(*ran) != 0 {
		t.Errorf("usage errors ran %q", *ran)
	}

	if err := k.command(nil); err != nil {
		t.Fatal(err)
	}
	if len(*ran) == 0 || (*ran)[0] != "setxkbmap" {
		t.Errorf("kbd with no command ran %q, want the rules loaded", *ran)
	}
	*ran = nil
	if err := k.command([]string{"set", "my-us-intl"}); err != nil {
		t.Fatal(err)
	}
	if want := []string{"xkbcomp -w 0 -I" + k.xkbDir + " " + filepath.Join(k.xkbDir, "my-us-intl") + " :0"}; !slices.Equal(*ran, want) {
		t.Errorf("kbd set ran %q, want %q", *ran, want)
	}
}

func TestWatchEvents(t *testing.T) {
	const header = `monitor will print the received events for:
UDEV - the event which udev sends out after rule processing

`
	const plug = `UDEV  [5131.612001] add      /devices/pci0000:00/0000:00:14.0/usb3/3-1/3-1:1.0/0003:05AC:024F.0007/input/input31 (input)
UDEV  [5131.613224] add      /devices/pci0000:00/0000:00:14.0/usb3/3-1/3-1:1.0/0003:05AC:024F.0007/input/input31/event20 (input)
UDEV  [5131.640870] add      /devices/pci0000:00/0000:00:14.0/usb3/3-1/3-1:1.1/0003:05AC:024F.0008/input/input32 (input)
`
	const unplug = `UDEV  [5140.100001] remove   /devices/pci0000:00/0000:00:14.0/usb3/3-1/3-1:1.0/0003:05AC:024F.0007/input/input31/event20 (input)
UDEV  [5140.100002] remove   /devices/pci0000:00/0000:00:14.0/usb3/3-1/3-1:1.0/0003:05AC:024F.0007/input/input31 (input)
`
	const debounce = 20 * time.Millisecond

	r, w := io.Pipe()
	loaded := make(chan struct{}, 10)
	done := make(chan error)
	go func() { done <- watchEvents(r, debounce, func() { loaded <- struct{}{} }) }()

	count := func() int {
		time.Sleep(5 * debounce)
		return len(loaded)
	}

	io.WriteString(w, header)
	if n := count(); n != 0 {
		t.Fatalf("loaded %d times after header, want 0", n)
	}
	io.WriteString(w, plug)
	if n := count(); n != 1 {
		t.Fatalf("loaded %d times after one plug, want 1", n)
	}
	io.WriteString(w, unplug)
	if n := count(); n != 1 {
		t.Fatalf("loaded %d times after unplug, want 1", n)
	}
	io.WriteString(w, plug)
	if n := count(); n != 2 {
		t.Fatalf("loaded %d times after second plug, want 2", n)
	}

	w.Close()
	if err := <-done; err != nil {
		t.Fatalf("watchEvents: %v", err)
	}
}
