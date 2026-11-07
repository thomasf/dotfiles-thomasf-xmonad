//go:build linux

package main

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
)

var runBlacklist = map[string]bool{
	"rofi-theme-selector": true,
}

var termCommands = map[string]bool{
	"agy":          true,
	"alsamixer":    true,
	"bluetoothctl": true,
	"btop":         true,
	"claude":       true,
	"ctop":         true,
	"dstat":        true,
	"htop":         true,
	"k9s":          true,
	"lazydocker":   true,
	"mpdplay":      true,
	"mtr":          true,
	"ncmpcpp":      true,
	"nmtui":        true,
	"nvtop":        true,
	"ping":         true,
	"pipemixer":    true,
	"sysz":         true,
	"top":          true,
}

var sudoTermCommands = map[string]bool{
	"aptitude":  true,
	"bandwhich": true,
	"iftop":     true,
	"iotop":     true,
	"nethogs":   true,
	"powertop":  true,
}

var webCommands = map[string]bool{
	"google-chrome": true,
	"w":             true,
}

var powerCommands = map[string]bool{
	"hibernate": true,
	"reboot":    true,
	"shutdown":  true,
	"suspend":   true,
}

type LaunchMode int

const (
	modeDefault LaunchMode = iota
	modeTerm
	modeSudoTerm
	modeHoldTerm
)

func runRun(args []string) error {
	fs := newFlagSet("run", "[-p prompt] [-lines n] [-width pct] [-term | -sudo | -hold] [cmd [args...]]", `
Run lets the user choose and launch an application or command with rofi,
listing executables found in $PATH with fast directory caching.

    Enter     launch the application
    Alt+t     run in a terminal window (term-run)
    Alt+s     run in a terminal window with sudo
    Alt+h     run in a terminal window in hold mode

Terminal tools and sudo utilities are automatically run in a terminal window.
Run starts commands in the directory of the current workspace if it has one.
`)
	prompt := fs.String("p", "launch", "rofi `prompt`")
	lines := fs.Int("lines", 25, "rofi `lines`")
	width := fs.Int("width", rofiWidth, "rofi `width` percent")
	flagTerm := fs.Bool("term", false, "force run in a terminal window")
	flagSudo := fs.Bool("sudo", false, "force run in a terminal window with sudo")
	flagHold := fs.Bool("hold", false, "force run in a terminal window in hold mode")
	fs.Parse(args)

	nFlags := 0
	for _, b := range []*bool{flagTerm, flagSudo, flagHold} {
		if *b {
			nFlags++
		}
	}
	if nFlags > 1 {
		return errors.New("cannot specify more than one of -term, -sudo, -hold")
	}

	mode := modeDefault
	switch {
	case *flagSudo:
		mode = modeSudoTerm
	case *flagHold:
		mode = modeHoldTerm
	case *flagTerm:
		mode = modeTerm
	}

	if fs.NArg() > 0 {
		return execTarget(fs.Arg(0), fs.Args()[1:], mode)
	}

	bins, err := pathBinaries()
	if err != nil {
		return err
	}
	loadFrecency(runHistoryPath()).sort(bins)

	rofiArgs := []string{
		"-dmenu",
		"-i",
		"-p", *prompt,
		"-l", strconv.Itoa(*lines),
		"-kb-custom-1", "Alt+t",
		"-kb-custom-2", "Alt+s",
		"-kb-custom-3", "Alt+h",
	}

	cmd, err := rofiCommand(*width, 0, "", rofiArgs...)
	if err != nil {
		return err
	}
	cmd.Stdin = strings.NewReader(strings.Join(bins, "\n"))
	out, err := cmd.Output()

	if key, ok := rofiCustomKey(err); ok {
		switch key {
		case 1:
			mode = modeTerm
		case 2:
			mode = modeSudoTerm
		case 3:
			mode = modeHoldTerm
		default:
			return errFalse
		}
	} else if err != nil {
		return errFalse
	}

	line := strings.TrimSpace(string(out))
	if line == "" {
		return nil
	}

	parts, err := splitCommand(line)
	if err != nil {
		notifyError("", err.Error())
		return err
	}
	if len(parts) == 0 {
		return nil
	}

	return execTarget(parts[0], parts[1:], mode)
}

func execTarget(cmd string, args []string, mode LaunchMode) error {
	loadFrecency(runHistoryPath()).record(cmd)
	chdirWorkspaceDir()

	var target string
	var targetArgs []string

	switch mode {
	case modeTerm:
		target = "term-run"
		targetArgs = append([]string{"--", cmd}, args...)
	case modeSudoTerm:
		target = "term-run"
		targetArgs = append([]string{"-n", cmd, "--", "sudo", cmd}, args...)
	case modeHoldTerm:
		target = "term-run"
		targetArgs = append([]string{"-m", "hold", "--", cmd}, args...)
	default:
		switch {
		case sudoTermCommands[cmd]:
			target = "term-run"
			targetArgs = append([]string{"-n", cmd, "--", "sudo", cmd}, args...)
		case termCommands[cmd]:
			target = "term-run"
			targetArgs = append([]string{"--", cmd}, args...)
		case webCommands[cmd]:
			target = "www"
			targetArgs = args
		case powerCommands[cmd]:
			target = "a." + cmd
			targetArgs = args
		default:
			target = cmd
			targetArgs = args
		}
	}

	if err := execLauncher(append([]string{target}, targetArgs...), nil); err != nil {
		notifyError("", fmt.Sprintf("%s: %v", target, err))
		return err
	}
	return nil
}

func chdirWorkspaceDir() {
	w, err := currentWorkspace()
	if err != nil {
		return
	}
	dir, err := workspaceDir(w, noPick)
	if err == nil && dir != "" {
		_ = os.Chdir(dir)
	}
}

func runCachePath() string {
	return xdgCacheFile("run_cache")
}

func pathBinaries() ([]string, error) {
	return pathBinariesFrom(os.Getenv("PATH"), runCachePath())
}

func pathBinariesFrom(pathEnv, cachePath string) ([]string, error) {
	if cachePath != "" {
		if bins, ok := readCachedBinaries(cachePath, pathEnv); ok {
			return bins, nil
		}
	}

	bins := scanPathBinaries(pathEnv)
	if cachePath != "" {
		writeCachedBinaries(cachePath, bins)
	}
	return bins, nil
}

func readCachedBinaries(cachePath, pathEnv string) ([]string, bool) {
	fi, err := os.Stat(cachePath)
	if err != nil || fi.IsDir() {
		return nil, false
	}
	cacheMod := fi.ModTime()

	for _, dir := range filepath.SplitList(pathEnv) {
		if dir == "" {
			continue
		}
		dfi, err := os.Stat(dir)
		if err != nil || !dfi.IsDir() {
			continue
		}
		if dfi.ModTime().After(cacheMod) {
			return nil, false
		}
	}

	data, err := os.ReadFile(cachePath)
	if err != nil {
		return nil, false
	}
	lines := strings.Split(string(data), "\n")
	var bins []string
	for _, l := range lines {
		l = strings.TrimSpace(l)
		if l != "" {
			bins = append(bins, l)
		}
	}
	return bins, true
}

func scanPathBinaries(pathEnv string) []string {
	seen := make(map[string]bool)
	var bins []string

	for _, dir := range filepath.SplitList(pathEnv) {
		if dir == "" {
			continue
		}
		entries, err := os.ReadDir(dir)
		if err != nil {
			continue
		}
		for _, entry := range entries {
			name := entry.Name()
			if strings.HasPrefix(name, ".") || seen[name] || runBlacklist[name] {
				continue
			}
			typ := entry.Type()
			if typ.IsRegular() {
				if info, err := entry.Info(); err == nil && info.Mode()&0o111 != 0 {
					seen[name] = true
					bins = append(bins, name)
				}
			} else if typ&os.ModeSymlink != 0 {
				if fi, err := os.Stat(filepath.Join(dir, name)); err == nil && !fi.IsDir() && fi.Mode()&0o111 != 0 {
					seen[name] = true
					bins = append(bins, name)
				}
			}
		}
	}
	slices.Sort(bins)
	return bins
}

func writeCachedBinaries(cachePath string, bins []string) {
	_ = writeAtomic(cachePath, []byte(strings.Join(bins, "\n")), 0o644)
}
