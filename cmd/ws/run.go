//go:build linux

package main

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"time"
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
	"pa-mixer":     true,
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

type launchMode int

const (
	modeDefault launchMode = iota
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
Run starts commands in the directory and environment of the current workspace
if it has one.
`)
	prompt := fs.String("p", "launch", "rofi `prompt`")
	lines := fs.Int("lines", 25, "rofi `lines`")
	width := fs.Int("width", rofiWidth, "rofi `width` percent")
	flagTerm := fs.Bool("term", false, "force run in a terminal window")
	flagSudo := fs.Bool("sudo", false, "force run in a terminal window with sudo")
	flagHold := fs.Bool("hold", false, "force run in a terminal window in hold mode")
	fs.Parse(args)

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
	sortByFrecency(bins, runHistoryPath())

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

	if ee, ok := errors.AsType[*exec.ExitError](err); ok {
		switch ee.ExitCode() {
		case 10:
			mode = modeTerm
		case 11:
			mode = modeSudoTerm
		case 12:
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
		notify("", true, err.Error())
		return err
	}
	if len(parts) == 0 {
		return nil
	}

	return execTarget(parts[0], parts[1:], mode)
}

func execTarget(cmd string, args []string, mode launchMode) error {
	recordFrecency(cmd, runHistoryPath())
	setupWorkspaceEnv()

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

	if err := execLauncher(target, targetArgs...); err != nil {
		notify("", true, fmt.Sprintf("%s: %v", target, err))
		return err
	}
	return nil
}

func setupWorkspaceEnv() {
	w, err := currentWorkspace()
	if err != nil {
		return
	}
	dir, err := workspaceDir(w, noPick)
	if err == nil && dir != "" {
		_ = os.Chdir(dir)
		os.Setenv("WS_DIR", dir)
	}
	suffix, _ := w.suffix()
	for k, v := range map[string]string{
		"WS_NAME":     string(w),
		"WS_PREFIX":   w.prefix(),
		"WS_PROJECT":  w.project(),
		"WS_WORKTREE": w.worktree(),
		"WS_SUFFIX":   suffix,
	} {
		os.Setenv(k, v)
	}
}

func runCachePath() string {
	cacheDir := os.Getenv("XDG_CACHE_HOME")
	if cacheDir == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return ""
		}
		cacheDir = filepath.Join(home, ".cache")
	}
	return filepath.Join(cacheDir, "ws", "run_cache")
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
	if err := os.MkdirAll(filepath.Dir(cachePath), 0o755); err != nil {
		return
	}
	tmp := cachePath + ".tmp"
	data := strings.Join(bins, "\n")
	if err := os.WriteFile(tmp, []byte(data), 0o644); err != nil {
		return
	}
	_ = os.Rename(tmp, cachePath)
}

func splitCommand(s string) ([]string, error) {
	var args []string
	var cur strings.Builder
	var inQuote rune
	var escaped bool

	for _, r := range s {
		if escaped {
			cur.WriteRune(r)
			escaped = false
			continue
		}
		if r == '\\' && inQuote != '\'' {
			escaped = true
			continue
		}
		if inQuote != 0 {
			if r == inQuote {
				inQuote = 0
			} else {
				cur.WriteRune(r)
			}
			continue
		}
		if r == '\'' || r == '"' {
			inQuote = r
			continue
		}
		if r == ' ' || r == '\t' || r == '\n' {
			if cur.Len() > 0 {
				args = append(args, cur.String())
				cur.Reset()
			}
			continue
		}
		cur.WriteRune(r)
	}
	if inQuote != 0 {
		return nil, fmt.Errorf("unclosed quote in %q", s)
	}
	if escaped {
		cur.WriteRune('\\')
	}
	if cur.Len() > 0 {
		args = append(args, cur.String())
	}
	return args, nil
}

func runHistoryPath() string {
	cacheDir := os.Getenv("XDG_CACHE_HOME")
	if cacheDir == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return ""
		}
		cacheDir = filepath.Join(home, ".cache")
	}
	return filepath.Join(cacheDir, "ws", "run_history")
}

type frecencyEntry struct {
	count int
	last  time.Time
}

func frecencyScore(e frecencyEntry, now time.Time) float64 {
	if e.count <= 0 {
		return 0
	}
	age := now.Sub(e.last)
	var weight float64
	switch {
	case age < 4*time.Hour:
		weight = 100
	case age < 24*time.Hour:
		weight = 80
	case age < 7*24*time.Hour:
		weight = 60
	case age < 30*24*time.Hour:
		weight = 40
	default:
		weight = 20
	}
	return float64(e.count) * weight
}

func loadFrecencyHistory(path string) map[string]frecencyEntry {
	hist := make(map[string]frecencyEntry)
	data, err := os.ReadFile(path)
	if err != nil {
		return hist
	}
	for _, line := range strings.Split(string(data), "\n") {
		fields := strings.Split(line, "\t")
		if len(fields) < 3 {
			continue
		}
		name := fields[0]
		count, err := strconv.Atoi(fields[1])
		if err != nil {
			continue
		}
		ts, err := strconv.ParseInt(fields[2], 10, 64)
		if err != nil {
			continue
		}
		hist[name] = frecencyEntry{
			count: count,
			last:  time.Unix(ts, 0),
		}
	}
	return hist
}

func saveFrecencyHistory(path string, hist map[string]frecencyEntry) {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return
	}
	var b strings.Builder
	for name, entry := range hist {
		fmt.Fprintf(&b, "%s\t%d\t%d\n", name, entry.count, entry.last.Unix())
	}
	tmp := path + ".tmp"
	if err := os.WriteFile(tmp, []byte(b.String()), 0o644); err != nil {
		return
	}
	_ = os.Rename(tmp, path)
}

func recordFrecency(cmd, historyPath string) {
	if historyPath == "" || cmd == "" {
		return
	}
	hist := loadFrecencyHistory(historyPath)
	entry := hist[cmd]
	entry.count++
	entry.last = time.Now()
	hist[cmd] = entry
	saveFrecencyHistory(historyPath, hist)
}

func sortByFrecency(bins []string, historyPath string) {
	if historyPath == "" {
		return
	}
	hist := loadFrecencyHistory(historyPath)
	if len(hist) == 0 {
		return
	}
	now := time.Now()
	scores := make(map[string]float64, len(hist))
	for name, entry := range hist {
		scores[name] = frecencyScore(entry, now)
	}

	slices.SortStableFunc(bins, func(a, b string) int {
		sa := scores[a]
		sb := scores[b]
		if sa != sb {
			if sb > sa {
				return 1
			}
			return -1
		}
		return strings.Compare(a, b)
	})
}
