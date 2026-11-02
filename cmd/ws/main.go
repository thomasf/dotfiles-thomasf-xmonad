//go:build linux

// Ws handles xmonad workspaces outside xmonad: their names, the
// directories they work in and what is started when one is opened.
//
// Usage:
//
//	ws name [-p | -s]
//	ws dir [-set | -unset] [-pick] [workspace]
//	ws action [workspace]
//	ws wt list [-l] [-pick] [workspace]
//	ws wt add [-branch branch] [-pick] [project@]name
//	ws wt init [-y] [-pick] [workspace]
//	ws wt remove [-force] [-y] [-pick] [[project@]name]
//	ws pick [-go] [-shift] [-empty] [-p prompt] [-columns n] [-lines n] [-worktrees]
//	ws go workspace
//	ws shift [workspace]
//	ws rename [name]
//	ws window
//	ws rofi [-width pct] [-height pct] [rofi arguments]
//	ws run [-lines n] [-p prompt] [-width pct] [-term | -sudo | -hold] [cmd [args...]]
package main

import (
	"errors"
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	"strings"
	"syscall"
)

// errFalse makes ws exit with status 1 without a message, as a
// test that fails does.
var errFalse = errors.New("false")

func main() {
	log.SetFlags(0)
	log.SetPrefix("ws: ")
	if len(os.Args) < 2 {
		usage()
	}
	var err error
	switch cmd, args := os.Args[1], os.Args[2:]; cmd {
	case "name":
		err = runName(args)
	case "dir":
		err = runDir(args)
	case "action":
		err = runAction(args)
	case "wt":
		err = runWt(args)
	case "pick":
		err = runPick(args)
	case "rofi":
		err = runRofi(args)
	case "go":
		err = runGo(args)
	case "shift":
		err = runShift(args)
	case "rename":
		err = runRename(args)
	case "window":
		err = runWindow(args)
	case "run":
		err = runRun(args)
	case "-h", "-help", "--help", "help":
		fmt.Fprint(os.Stdout, usageText)
		os.Exit(0)
	default:
		usage()
	}
	if errors.Is(err, errFalse) {
		os.Exit(1)
	}
	if err != nil {
		log.Fatal(err)
	}
}

const usageText = `usage: ws name [-p | -s]
       ws dir [-set | -unset] [-pick] [workspace]
       ws action [workspace]
       ws wt list [-l] [-pick] [workspace]
       ws wt add [-branch branch] [-pick] [project@]name
       ws wt init [-y] [-pick] [workspace]
       ws wt remove [-force] [-y] [-pick] [[project@]name]
       ws pick [-go] [-shift] [-empty] [-p prompt] [-columns n] [-lines n] [-worktrees]
       ws go workspace
       ws shift [workspace]
       ws rename [name]
       ws window
       ws rofi [-width pct] [-height pct] [rofi arguments]
       ws run [-lines n] [-p prompt] [-width pct] [-term | -sudo | -hold] [cmd [args...]]

Run ws <command> -h for help on a command.
`

func usage() {
	fmt.Fprint(os.Stderr, usageText)
	os.Exit(2)
}

// newFlagSet returns the flag set of command name, taking arguments
// args. On -h, or by usageError, it prints the usage line, help and
// flags of the command.
func newFlagSet(name, args, help string) *flag.FlagSet {
	fs := flag.NewFlagSet("ws "+name, flag.ExitOnError)
	fs.Usage = func() {
		w := fs.Output()
		fmt.Fprintf(w, "usage: %s\n\n%s\n", strings.TrimSpace(fs.Name()+" "+args), strings.TrimSpace(help))
		hasFlags := false
		fs.VisitAll(func(*flag.Flag) { hasFlags = true })
		if hasFlags {
			fmt.Fprintln(w, "\nFlags:")
			fs.PrintDefaults()
		}
	}
	return fs
}

// usageError prints the usage of the command of fs and exits with
// status 2.
func usageError(fs *flag.FlagSet) {
	fs.Usage()
	os.Exit(2)
}

// checkArgs verifies that fs has between minArgs and maxArgs arguments.
// If maxArgs is negative, no upper bound is enforced. On violation, it calls usageError.
func checkArgs(fs *flag.FlagSet, minArgs, maxArgs int) {
	n := fs.NArg()
	if n < minArgs || (maxArgs >= 0 && n > maxArgs) {
		usageError(fs)
	}
}

// output prints s, followed by a newline only on a terminal: callers
// such as Emacs's shell-command-to-string use the output as it is.
func output(s string) {
	fmt.Print(s)
	if isTerminal(os.Stdout) {
		fmt.Println()
	}
}

// execLauncher replaces ws with the program name from $PATH.
func execLauncher(name string, args ...string) error {
	path, err := exec.LookPath(name)
	if err != nil {
		return err
	}
	return syscall.Exec(path, append([]string{name}, args...), os.Environ())
}
