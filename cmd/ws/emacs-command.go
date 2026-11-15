//go:build linux

package main

import (
	"bytes"
	"cmp"
	"context"
	"encoding/base64"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"sync"
	"text/tabwriter"
	"time"
)

const emacsUsage = `list [-global | -visible | target]
       ws emacs [-noheader] [-noredact] [-global] [target] (region | visible)`

const emacsHelp = `
Emacs prints text from Emacs servers or lists them. By default it addresses
the server named after the file name of the workspace prefix (see ws name -p),
for the workspace that contextWorkspace resolves: that of $WS_WORKSPACE,
which ws term sets, or else the current one.

    list       list running Emacs servers with their workspace, PID,
               working directory, and selected buffer
    region     the marked text: the active region of the selected
               window, else of another window on a visible frame,
               after a line naming its file and lines. If nothing is
               marked in the workspace's Emacs, other visible Emacs
               servers are checked.
    visible    the text of every window on the visible frames across
               all running Emacs servers, each after a line naming
               its buffer
    global     the text of all windows on all frames across all
               running Emacs servers

With -global, visible and region inspect all running Emacs servers,
not just those on visible workspaces.

When multiple servers have text to show, each server's section begins
with a line '==> emacs:<server> <=='.

Emacs redacts secrets via delemark unless -noredact is given.
`

const emacsAllFramesExpr = `(let ((text ""))
  (dolist (frame (frame-list))
    (when (null (frame-parent frame))
      (dolist (window (window-list frame (quote no-minibuffer)))
        (with-current-buffer (window-buffer window)
          (setq text (concat text
                             (format "\n--- Buffer: %s ---\n" (buffer-name))
                             (buffer-substring-no-properties
                              (window-start window)
                              (window-end window))))))))
  text)`

func runEmacs(args []string) error {
	if len(args) > 0 && args[0] == "list" {
		return runEmacsList(args[1:])
	}

	fs := newFlagSet("emacs", emacsUsage, emacsHelp)
	noHeader := fs.Bool("noheader", false, "print the region without the line naming its file and lines")
	noRedact := fs.Bool("noredact", false, "do not redact secrets via delemark")
	global := fs.Bool("global", false, "collect from all Emacs servers, not just visible ones")
	fs.Parse(args)
	checkArgs(fs, 1, 2)

	var cmd, target string
	for _, arg := range fs.Args() {
		switch arg {
		case "region", "visible", "global":
			if cmd != "" {
				usageError(fs)
			}
			cmd = arg
		default:
			if target != "" {
				usageError(fs)
			}
			target = arg
		}
	}
	if cmd == "" {
		usageError(fs)
	}
	if cmd == "global" {
		*global = true
		cmd = "visible"
	}

	if target != "" {
		var err error
		target, err = resolveEmacsTarget(target)
		if err != nil {
			return err
		}
	}

	switch cmd {
	case "region":
		return runEmacsRegion(target, *noHeader, *noRedact, *global)
	case "visible":
		return runEmacsVisible(target, *noRedact, *global)
	default:
		usageError(fs)
		return nil
	}
}

func runEmacsList(args []string) error {
	fs := newFlagSet("emacs list", "[-global | -visible | target]", `
List prints running Emacs servers, by default that of the current
workspace prefix. With -global it lists every running Emacs server,
and with -visible those on the visible workspaces.

The columns are the server name, workspace, process ID, working directory,
and selected buffer.
`)
	global := fs.Bool("global", false, "list every running Emacs server")
	visible := fs.Bool("visible", false, "list the Emacs servers on the visible workspaces")
	fs.Parse(args)
	checkArgs(fs, 0, 1)
	if *global && *visible || (*global || *visible) && fs.NArg() > 0 {
		usageError(fs)
	}

	servers := runningEmacsServers()
	if len(servers) == 0 {
		return errors.New("emacs: no running Emacs servers found")
	}

	switch {
	case !*global && !*visible:
		target := fs.Arg(0)
		if target == "" {
			w, err := contextWorkspace()
			if err != nil {
				return err
			}
			target = fileName(w.prefix())
		} else {
			var err error
			target, err = resolveEmacsTarget(target)
			if err != nil {
				return err
			}
		}
		servers = slices.DeleteFunc(servers, func(s string) bool { return s != target })
		if len(servers) == 0 {
			return fmt.Errorf("no Emacs server for %s", target)
		}
	}

	ordered := orderEmacsServers(servers)

	type emacsServerInfo struct {
		name      string
		workspace string
		pid       string
		cwd       string
		buffer    string
		visible   bool
	}

	const metaExpr = `(format "%d\t%s\t%s\t%s" (emacs-pid) (if (visible-frame-list) "yes" "no") (abbreviate-file-name default-directory) (buffer-name (window-buffer (selected-window))))`
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()

	infos := make([]*emacsServerInfo, len(ordered))
	var wg sync.WaitGroup
	for i, s := range ordered {
		wg.Add(1)
		go func(idx int, server string) {
			defer wg.Done()
			out, err := emacsEval(ctx, server, metaExpr)
			if err != nil {
				return
			}
			parts := strings.Split(out, "\t")
			if len(parts) < 4 {
				return
			}
			infos[idx] = &emacsServerInfo{
				name:    server,
				pid:     parts[0],
				visible: parts[1] == "yes",
				cwd:     shortenHome(parts[2]),
				buffer:  parts[3],
			}
		}(i, s)
	}
	wg.Wait()

	var list []*emacsServerInfo
	for _, info := range infos {
		if info != nil {
			if *visible && !info.visible {
				continue
			}
			list = append(list, info)
		}
	}

	if len(list) == 0 {
		if *visible {
			return nil
		}
		return errors.New("emacs: no running Emacs servers found")
	}

	names, _, _ := desktops()
	var wins []Window
	if x, err := openX(); err == nil {
		defer x.close()
		wins, _ = x.listWindows(names, nil)
	}

	tw := tabwriter.NewWriter(os.Stdout, 0, 4, 2, ' ', 0)
	fmt.Fprintln(tw, "NAME\tWORKSPACE\tPID\tCWD\tBUFFER")
	for _, info := range list {
		ws := ""
		if pid, err := strconv.Atoi(info.pid); err == nil && pid > 0 {
			if i := slices.IndexFunc(wins, func(w Window) bool { return w.pid == pid }); i >= 0 {
				ws = wins[i].workspace
			}
		}
		if ws == "" {
			if i := slices.IndexFunc(names, func(n string) bool { return fileName(Workspace(n).prefix()) == info.name }); i >= 0 {
				ws = names[i]
			} else {
				ws = info.name
			}
		}
		info.workspace = ws
		fmt.Fprintf(tw, "%s\t%s\t%s\t%s\t%s\n", info.name, info.workspace, info.pid, info.cwd, info.buffer)
	}
	return tw.Flush()
}

func resolveEmacsTarget(target string) (string, error) {
	if strings.HasPrefix(target, ".") {
		w, err := contextWorkspace()
		if err != nil {
			return "", err
		}
		target = w.prefix() + target
	}
	return fileName(Workspace(target).prefix()), nil
}

func runEmacsRegion(target string, noHeader, noRedact, global bool) error {
	expr := fmt.Sprintf("(my-region-text %s)", map[bool]string{true: "t", false: "nil"}[noHeader])
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()

	if target != "" {
		text, err := emacsEval(ctx, target, expr)
		if err != nil {
			return err
		}
		return printEmacsText(text, noRedact)
	}

	servers := runningEmacsServers()
	ordered := orderEmacsServers(servers)
	if len(ordered) == 0 {
		if w, err := contextWorkspace(); err == nil {
			return fmt.Errorf("emacs: no Emacs server %s", fileName(w.prefix()))
		}
		return errors.New("emacs: no running Emacs servers found")
	}

	// Try each server in order; return the first one with marked text.
	for _, s := range ordered {
		text, err := emacsEval(ctx, s, expr)
		if err == nil && text != "" {
			return printEmacsText(text, noRedact)
		}
	}
	return errors.New("emacs: No marked text")
}

func runEmacsVisible(target string, noRedact, global bool) error {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()

	expr := "(my-visible-text)"
	if global {
		expr = emacsAllFramesExpr
	}

	if target != "" {
		text, err := emacsEval(ctx, target, expr)
		if err != nil {
			return err
		}
		return printEmacsText(text, noRedact)
	}

	servers := runningEmacsServers()
	if len(servers) == 0 {
		if w, err := contextWorkspace(); err == nil {
			return fmt.Errorf("emacs: no Emacs server %s", fileName(w.prefix()))
		}
		return errors.New("emacs: no running Emacs servers found")
	}
	ordered := orderEmacsServers(servers)

	type serverResult struct {
		server string
		text   string
	}
	results := make([]serverResult, len(ordered))
	var wg sync.WaitGroup

	for i, s := range ordered {
		wg.Add(1)
		go func(idx int, server string) {
			defer wg.Done()
			txt, err := emacsEval(ctx, server, expr)
			if err == nil && strings.TrimSpace(txt) != "" {
				results[idx] = serverResult{server: server, text: txt}
			}
		}(i, s)
	}
	wg.Wait()

	var active []serverResult
	for _, r := range results {
		if r.text != "" {
			active = append(active, r)
		}
	}

	for i, r := range active {
		if i > 0 {
			fmt.Println()
		}
		if len(active) > 1 || global {
			fmt.Printf("==> emacs:%s <==\n", r.server)
		}
		text := r.text
		if !noRedact {
			text = redact(text)
		}
		fmt.Print(text)
		if !strings.HasSuffix(text, "\n") {
			fmt.Println()
		}
	}
	return nil
}

func printEmacsText(text string, noRedact bool) error {
	if !noRedact {
		text = redact(text)
	}
	fmt.Print(text)
	if text != "" && !strings.HasSuffix(text, "\n") {
		fmt.Println()
	}
	return nil
}

func emacsSocketDirs() []string {
	var dirs []string
	seen := make(map[string]bool)
	add := func(dir string) {
		if dir != "" && !seen[dir] {
			seen[dir] = true
			dirs = append(dirs, dir)
		}
	}
	if xdg := os.Getenv("XDG_RUNTIME_DIR"); xdg != "" {
		add(filepath.Join(xdg, "emacs"))
	} else {
		add(fmt.Sprintf("/run/user/%d/emacs", os.Getuid()))
	}
	add(fmt.Sprintf("/tmp/emacs%d", os.Getuid()))
	return dirs
}

func runningEmacsServers() []string {
	var servers []string
	seen := make(map[string]bool)
	for _, dir := range emacsSocketDirs() {
		entries, err := os.ReadDir(dir)
		if err != nil {
			continue
		}
		for _, e := range entries {
			name := e.Name()
			if seen[name] {
				continue
			}
			info, err := e.Info()
			if err != nil {
				continue
			}
			if info.Mode()&os.ModeSocket != 0 {
				seen[name] = true
				servers = append(servers, name)
			}
		}
	}
	slices.Sort(servers)
	return servers
}

func orderEmacsServers(servers []string) []string {
	curPrefix := ""
	if w, err := contextWorkspace(); err == nil {
		curPrefix = fileName(w.prefix())
	}

	serverRank := make(map[string]int)
	for _, s := range servers {
		serverRank[s] = 1000000
	}

	if x, err := openX(); err == nil {
		defer x.close()
		if names, cur, err := x.desktops(); err == nil {
			shown := x.visibleDesktops()
			if shown == nil {
				shown = make(map[int]bool)
			}
			shown[cur] = true
			for i, name := range names {
				if shown[i] {
					p := fileName(Workspace(name).prefix())
					r := rank(i, cur)
					if current, ok := serverRank[p]; !ok || r < current {
						serverRank[p] = r
					}
				}
			}
		}
	} else if curPrefix != "" {
		serverRank[curPrefix] = 0
	}

	if curPrefix != "" {
		serverRank[curPrefix] = 0
	}

	sorted := slices.Clone(servers)
	slices.SortStableFunc(sorted, func(a, b string) int {
		ra, rb := serverRank[a], serverRank[b]
		if ra != rb {
			return cmp.Compare(ra, rb)
		}
		return cmp.Compare(a, b)
	})
	return sorted
}

// emacsEval evaluates expr, which must return a string, in the Emacs
// server named server and returns the string. The string travels base64
// encoded, as emacsclient prints values as Lisp, quoted and escaped.
func emacsEval(ctx context.Context, server, expr string) (string, error) {
	expr = fmt.Sprintf("(base64-encode-string (encode-coding-string %s 'utf-8 t) t)", expr)
	// --alternate-editor=false keeps emacsclient from starting a daemon
	// when there is no server, as an empty $ALTERNATE_EDITOR would.
	cmd := exec.CommandContext(ctx, "emacsclient", "--alternate-editor=false", "-s", server, "-e", expr)
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		if !errors.As(err, new(*exec.ExitError)) {
			return "", err
		}
		msg := strings.TrimPrefix(strings.TrimSpace(stderr.String()), "*ERROR*: ")
		if msg == "" {
			msg = "no Emacs server " + server
		}
		return "", errors.New("emacs: " + msg)
	}
	b, err := base64.StdEncoding.DecodeString(string(bytes.Trim(bytes.TrimSpace(out), `"`)))
	if err != nil {
		return "", fmt.Errorf("emacsclient: %w", err)
	}
	return string(b), nil
}
