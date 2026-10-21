//go:build linux

package main

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
)

func runEnv(args []string) error {
	fs := newFlagSet("env", "[-file] [workspace]", `
Env prints the environment file of a workspace, by default the current
one, for a shell to evaluate:

    eval "$(ws env)"

The file is the shell script in ~/.config/wsenv named like the
directory of the workspace, or like its prefix if it has none. Without
-file, env exits with status 1 if the file does not exist.
`)
	file := fs.Bool("file", false, "print the name of the environment file")
	fs.Parse(args)
	w, err := workspaceArg(fs)
	if err != nil {
		return err
	}
	name, err := envFile(w)
	if err != nil {
		return err
	}
	if *file {
		output(name)
		return nil
	}
	b, err := readEnv(name)
	if err != nil {
		return err
	}
	_, err = os.Stdout.Write(b)
	return err
}

// envFile returns the environment file of w, in ~/.config/wsenv: the
// file named like the directory w works in, or like its prefix if it
// has none.
func envFile(w workspace) (string, error) {
	name := w.prefix()
	dir, err := workspaceDir(w, noPick)
	switch {
	case err == nil:
		name = filepath.Base(dir)
	case !errors.Is(err, errNoDir):
		return "", err
	}
	return configFile("wsenv", name)
}

// readEnv returns the contents of the environment file name. It
// returns errFalse if there is none.
func readEnv(name string) ([]byte, error) {
	b, err := os.ReadFile(name)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, errFalse
	}
	return b, err
}
