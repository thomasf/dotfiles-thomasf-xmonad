//go:build linux

// Ws handles xmonad workspaces outside xmonad: their names, the
// directories they work in and what is started when one is opened.
//
// Usage:
//
//	ws [-v] name [-d] [-p | -s]
//	ws [-v] dir <command> [arguments]
//	ws [-v] action [workspace]
//	ws [-v] wt list [-l] [-pick] [workspace]
//	ws [-v] wt add [-branch branch] [-pick] [project@]name
//	ws [-v] wt init [-y] [-pick] [workspace]
//	ws [-v] wt remove [-force] [-y] [-pick] [[project@]name]
//	ws [-v] pick [-go] [-shift] [-empty] [-worktrees] [-dirs] [-p prompt] [-lines n]
//	ws [-v] go workspace
//	ws [-v] shift [workspace]
//	ws [-v] fill [workspace]
//	ws [-v] clean
//	ws [-v] rename [name]
//	ws [-v] window
//	ws [-v] rofi [-width pct] [-height pct] [rofi arguments]
//	ws [-v] run [-lines n] [-p prompt] [-width pct] [-term | -sudo | -hold] [cmd [args...]]
//	ws [-v] term [terminal arguments]
//	ws [-v] emacs list [-global | -visible | target]
//	ws [-v] emacs read [-lines n | -full] [-no-header] [-no-redact] [-ansi] [-no-trim] [-region] [-except target] (-global | -visible)
//	ws [-v] emacs read [-lines n | -full] [-no-header] [-no-redact] [-ansi] [-no-trim] [-region] target...
//	ws [-v] rc list [-global | -visible | target]
//	ws [-v] rc send [-windows] [-paste] [-cr] target [text...]
//	ws [-v] rc read [-lines n | -full] [-no-redact] [-ansi] [-no-trim] [-except target] (-global | -visible)
//	ws [-v] rc read [-lines n | -full] [-no-redact] [-ansi] [-no-trim] [-windows] target...
//	ws [-v] read [-lines n | -full] [-no-redact] [-ansi] [-no-trim] [-region] [-no-header] [-except target] [-emacs] [-rc] [-global | -visible | target...]
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

var verbose bool

func init() {
	log.SetFlags(0)
	log.SetPrefix("ws: ")
}

func isVerbose() bool {
	return verbose || os.Getenv("WS_VERBOSE") == "1" || os.Getenv("WS_VERBOSE") == "true"
}

func vlog(format string, args ...any) {
	if isVerbose() {
		log.Printf(format, args...)
	}
}

// errFalse makes ws exit with status 1 without a message, as a
// test that fails does.
var errFalse = errors.New("false")

func newWsCommand() *ffcli.Command {
	fs := flag.NewFlagSet("ws", flag.ContinueOnError)
	fs.BoolVar(&verbose, "v", false, "log what the command is doing and issues encountered")
	return &ffcli.Command{
		Name:       "ws",
		ShortUsage: "ws [-v] <command> [arguments]",
		ShortHelp:  "handle xmonad workspaces outside xmonad",
		LongHelp: `Ws handles xmonad workspaces outside xmonad: their names, the
directories they work in and what is started when one is opened.`,
		FlagSet:   fs,
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

// prepareArgs adjusts args for commands that pass arbitrary flags through,
// taking into account root flags like -v.
func prepareArgs(args []string) []string {
	subIdx := -1
	for i, arg := range args {
		if arg == "-v" || arg == "--v" {
			continue
		}
		if strings.HasPrefix(arg, "-v=") || strings.HasPrefix(arg, "--v=") {
			continue
		}
		subIdx = i
		break
	}
	if subIdx == -1 || subIdx >= len(args)-1 {
		return args
	}
	switch args[subIdx] {
	case "term", "rofi":
		next := args[subIdx+1]
		if next == "--" {
			return args
		}
		switch next {
		case "-h", "-help", "--help", "help":
			return args
		default:
			result := make([]string, 0, len(args)+1)
			result = append(result, args[:subIdx+1]...)
			result = append(result, "--")
			result = append(result, args[subIdx+1:]...)
			return result
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
	vlog("executing ws %s", strings.Join(args, " "))
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
