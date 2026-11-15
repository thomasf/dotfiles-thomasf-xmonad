//go:build linux

// Ws handles xmonad workspaces outside xmonad: their names, the
// directories they work in and what is started when one is opened.
//
// Usage:
//
//	ws name [-d] [-p | -s]
//	ws dir [-set | -unset] [-pick] [workspace]
//	ws action [workspace]
//	ws wt list [-l] [-pick] [workspace]
//	ws wt add [-branch branch] [-pick] [project@]name
//	ws wt init [-y] [-pick] [workspace]
//	ws wt remove [-force] [-y] [-pick] [[project@]name]
//	ws pick [-go] [-shift] [-empty] [-p prompt] [-lines n] [-worktrees]
//	ws go workspace
//	ws shift [workspace]
//	ws fill [workspace]
//	ws clean
//	ws rename [name]
//	ws window
//	ws rofi [-width pct] [-height pct] [rofi arguments]
//	ws run [-lines n] [-p prompt] [-width pct] [-term | -sudo | -hold] [cmd [args...]]
//	ws term [terminal arguments]
//	ws emacs list [-global | -visible | target]
//	ws emacs read [-lines n | -full] [-no-header] [-no-redact] [-ansi] [-no-trim] [-region] [-except target] (-global | -visible)
//	ws emacs read [-lines n | -full] [-no-header] [-no-redact] [-ansi] [-no-trim] [-region] target...
//	ws rc list [-global | -visible | target]
//	ws rc send [-windows] [-paste] [-cr] target [text...]
//	ws rc read [-lines n | -full] [-no-redact] [-ansi] [-no-trim] [-except target] (-global | -visible)
//	ws rc read [-lines n | -full] [-no-redact] [-ansi] [-no-trim] [-windows] target...
//	ws read [-lines n | -full] [-no-redact] [-ansi] [-no-trim] [-region] [-no-header] [-except target] [-emacs] [-rc] [-global | -visible | target...]
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"log"
	"os"
	"strings"
	"text/tabwriter"

	"github.com/peterbourgon/ff/v3/ffcli"
)

// errFalse makes ws exit with status 1 without a message, as a
// test that fails does.
var errFalse = errors.New("false")

func newWsCommand() *ffcli.Command {
	return &ffcli.Command{
		Name:       "ws",
		ShortUsage: "ws <command> [arguments]",
		ShortHelp:  "handle xmonad workspaces outside xmonad",
		LongHelp: `Ws handles xmonad workspaces outside xmonad: their names, the
directories they work in and what is started when one is opened.`,
		UsageFunc: commandUsage,
		Subcommands: []*ffcli.Command{
			newNameCommand(),
			newDirCommand(),
			newActionCommand(),
			newWtCommand(),
			newPickCommand(),
			newRofiCommand(),
			newGoCommand(),
			newShiftCommand(),
			newFillCommand(),
			newCleanCommand(),
			newRenameCommand(),
			newWindowCommand(),
			newRunCommand(),
			newTermCommand(),
			newEmacsCommand(),
			newRcCommand(),
			newReadCommand(),
		},
	}
}

func main() {
	log.SetFlags(0)
	log.SetPrefix("ws: ")
	root := newWsCommand()
	if err := runFFCLI(root, os.Args[1:]); err != nil {
		if errors.Is(err, errFalse) {
			os.Exit(1)
		}
		log.Fatal(err)
	}
}

// usageError prints the usage of the command of fs and exits with
// status 2.
func usageError(fs *flag.FlagSet) {
	fs.Usage()
	os.Exit(2)
}

// output prints s, followed by a newline only on a terminal: callers
// such as Emacs's shell-command-to-string use the output as it is.
func output(s string) {
	fmt.Print(s)
	if isTerminal(os.Stdout) {
		fmt.Println()
	}
}

// prepareArgs adjusts args for commands that pass arbitrary flags through.
func prepareArgs(args []string) []string {
	if len(args) < 2 {
		return args
	}
	switch args[0] {
	case "term", "rofi":
		if args[1] == "--" {
			return args
		}
		switch args[1] {
		case "-h", "-help", "--help", "help":
			return args
		default:
			return append([]string{args[0], "--"}, args[1:]...)
		}
	}
	return args
}

// runFFCLI parses and executes a hierarchical ffcli.Command tree.
func runFFCLI(cmd *ffcli.Command, args []string) error {
	args = prepareArgs(args)
	var noExec ffcli.NoExecError
	err := cmd.Parse(args)
	if errors.Is(err, flag.ErrHelp) {
		os.Exit(0)
	}
	if errors.As(err, &noExec) {
		fmt.Fprint(os.Stderr, noExec.Command.UsageFunc(noExec.Command))
		os.Exit(2)
	}
	if err != nil {
		os.Exit(2)
	}
	err = cmd.Run(context.Background())
	if errors.Is(err, flag.ErrHelp) {
		os.Exit(0)
	}
	if errors.As(err, &noExec) {
		fmt.Fprint(os.Stderr, noExec.Command.UsageFunc(noExec.Command))
		os.Exit(2)
	}
	return err
}

// commandUsage formats command usage, long help, subcommands, and flags
// in the coherent ws style.
func commandUsage(c *ffcli.Command) string {
	var b strings.Builder
	fmt.Fprintf(&b, "usage: %s\n\n%s\n", strings.TrimSpace(c.ShortUsage), strings.TrimSpace(c.LongHelp))
	if len(c.Subcommands) > 0 {
		fmt.Fprintf(&b, "\nCommands:\n")
		tw := tabwriter.NewWriter(&b, 0, 4, 2, ' ', 0)
		for _, sub := range c.Subcommands {
			fmt.Fprintf(tw, "    %s\t%s\n", sub.Name, sub.ShortHelp)
		}
		tw.Flush()
		if c.Name == "ws" {
			fmt.Fprintf(&b, "\nRun ws <command> -h for help on a command.\n")
		} else {
			fmt.Fprintf(&b, "\nRun ws %s <command> -h for help on a command.\n", c.Name)
		}
	}
	hasFlags := false
	if c.FlagSet != nil {
		c.FlagSet.VisitAll(func(*flag.Flag) { hasFlags = true })
		if hasFlags {
			fmt.Fprintln(&b, "\nFlags:")
			oldOutput := c.FlagSet.Output()
			c.FlagSet.SetOutput(&b)
			c.FlagSet.PrintDefaults()
			c.FlagSet.SetOutput(oldOutput)
		}
	}
	return strings.TrimSpace(b.String()) + "\n"
}
