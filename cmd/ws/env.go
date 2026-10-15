//go:build linux

package main

import (
	"errors"
	"flag"
	"io/fs"
	"os"
	"path/filepath"
)

func runEnv(args []string) error {
	fs := flag.NewFlagSet("env", flag.ExitOnError)
	file := fs.Bool("file", false, "print the name of the environment file")
	fs.Parse(args)
	w, err := workspaceArg(fs.Args())
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
