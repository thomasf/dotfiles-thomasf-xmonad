//go:build linux

package main

import "errors"

func runName(args []string) error {
	fs := newFlagSet("name", "[-d] [-p | -s]", `
Name prints the name of the current workspace, in the form used for
file names, sockets and the critty remote control name: letters, digits
and "._@+-" are kept and every other character becomes "_", as does a
leading "-". -d prints it as xmonad shows it. A workspace name is
<prefix>[.<suffix>], and the prefix is <project>[@<worktree>]. In
workspace hello@test.bot, the project is hello, the worktree test and
the suffix bot.

Output to a terminal ends in a newline; other output does not, so that
it can be used as it is.
`)
	prefix := fs.Bool("p", false, "print the prefix")
	suffix := fs.Bool("s", false, "print the suffix, or exit with status 1 if there is none")
	display := fs.Bool("d", false, "print the name as xmonad shows it")
	fs.Parse(args)
	checkArgs(fs, 0, 0)
	if *prefix && *suffix {
		return errors.New("cannot specify both -p and -s")
	}
	w, err := currentWorkspace()
	if err != nil {
		return err
	}
	s := string(w)
	switch {
	case *prefix:
		s = w.prefix()
	case *suffix:
		var ok bool
		if s, ok = w.suffix(); !ok {
			return errFalse
		}
	}
	if !*display {
		s = fileName(s)
	}
	output(s)
	return nil
}
