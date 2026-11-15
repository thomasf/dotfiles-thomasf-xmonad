//go:build linux

package main

import (
	"context"
	"errors"
	"log"
	"maps"
	"os"
	"os/exec"
	"slices"
	"strconv"
	"strings"

	critty "github.com/go-pa/critty-ipc"
	"github.com/peterbourgon/ff/v3/ffcli"
)

// terminals are the terminal emulators runTerm starts, in order of preference.
var terminals = []string{"critty", "alacritty", "kitty", "urxvt", "ptyxis", "xterm"}

var (
	// remoteControlSuffixes are the suffixes of the workspaces whose terminals
	// accept remote control.
	remoteControlSuffixes = []string{"bot", "logs", "test"}
	alwaysRemoteControl   = true
)

func newTermCommand() *ffcli.Command {
	return &ffcli.Command{
		Name:       "term",
		ShortUsage: "ws term [terminal arguments]",
		ShortHelp:  "start a terminal emulator in the current workspace",
		LongHelp: `Term starts the first terminal emulator found in $PATH, passing it args
unparsed. It sets WS_WORKSPACE to the current workspace, where the
terminal opens. On a workspace with a suffix in .bot, .logs, or .test
it lets critty accept remote control.`,
		UsageFunc: commandUsage,
		Exec: func(ctx context.Context, args []string) error {
			return runTerm(args)
		},
	}
}

// runTerm starts the first terminal emulator found in $PATH, passing it args
// unparsed. It sets WS_WORKSPACE to the current workspace, where the
// terminal opens; see [contextWorkspace]. On a workspace with a suffix in
// remoteControlSuffixes it lets critty accept remote control; see
// [remoteControlName].
func runTerm(args []string) error {
	env := make(map[string]string)
	if w, err := currentWorkspace(); err != nil {
		log.Print(err)
	} else {
		env["WS_WORKSPACE"] = string(w)
		if suffix, _ := w.suffix(); os.Getenv("CRITTY_REMOTE_CONTROL") == "" &&
			(alwaysRemoteControl || slices.Contains(remoteControlSuffixes, suffix)) {
			maps.Copy(env, remoteControlEnv(w))
		}
	}
	for _, name := range terminals {
		if _, err := exec.LookPath(name); err == nil {
			return execLauncher(append([]string{name}, args...), env)
		}
	}
	return errors.New("no terminal found: " + strings.Join(terminals, ", "))
}

// remoteControlEnv returns the environment that makes critty accept remote
// control under a name for workspace w that no running critty uses.
func remoteControlEnv(w Workspace) map[string]string {
	taken, err := critty.RemoteControlNames()
	if err != nil {
		log.Print(err)
		return nil
	}
	return map[string]string{"CRITTY_REMOTE_CONTROL": remoteControlName(w, taken)}
}

// remoteControlName returns the remote control name for a terminal on
// workspace w that is not in taken: the file name of w for the first
// terminal, then <file name>+2, +3 and so on, the lowest number free.
func remoteControlName(w Workspace, taken []string) string {
	base := w.fileName()
	name := base
	for n := 2; slices.Contains(taken, name); n++ {
		name = base + "+" + strconv.Itoa(n)
	}
	return name
}
