//go:build linux

package main

import (
	"bytes"
	"cmp"
	"context"
	_ "embed"
	"encoding/base64"
	"errors"
	"flag"
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

	"github.com/peterbourgon/ff/v3/ffcli"
)

//go:embed elisp/server-meta.el
var elispServerMeta string

//go:embed elisp/buffer-text.el
var elispBufferText string

//go:embed elisp/extract-viewport.el
var elispExtractViewport string

//go:embed elisp/extract-full.el
var elispExtractFull string

//go:embed elisp/extract-lines.el
var elispExtractLines string

//go:embed elisp/region-text.el
var elispRegionText string

//go:embed elisp/eval-wrapper.el
var elispEvalWrapper string

func emacsTextExpr(full bool, lines int, allFrames bool) string {
	frameList := "(visible-frame-list)"
	if allFrames {
		frameList = "(frame-list)"
	}

	substringExpr := strings.TrimSpace(elispExtractViewport)
	if full {
		substringExpr = strings.TrimSpace(elispExtractFull)
	} else if lines > 0 {
		substringExpr = strings.TrimSpace(fmt.Sprintf(elispExtractLines, lines))
	}

	return fmt.Sprintf(strings.TrimSpace(elispBufferText), frameList, substringExpr)
}

func newEmacsCommand() *ffcli.Command {
	return &ffcli.Command{
		Name:       "emacs",
		ShortUsage: "ws emacs <command> [arguments]",
		ShortHelp:  "work with running Emacs servers",
		LongHelp: `Emacs works with running Emacs servers. By default it addresses
the server named after the file name of the workspace prefix (see ws name -p),
for the workspace that contextWorkspace resolves: that of $WS_WORKSPACE,
which ws term sets, or else the current one.`,
		UsageFunc: commandUsage,
		Subcommands: []*ffcli.Command{
			newEmacsListCommand(),
			newEmacsReadCommand(),
		},
	}
}

func newEmacsListCommand() *ffcli.Command {
	fs := flag.NewFlagSet("ws emacs list", flag.ContinueOnError)
	global := fs.Bool("global", false, "list every running Emacs server")
	visible := fs.Bool("visible", false, "list the Emacs servers on the visible workspaces")
	cmd := &ffcli.Command{
		Name:       "list",
		ShortUsage: "ws emacs list [-global | -visible | target]",
		ShortHelp:  "list running Emacs servers with workspace, PID, CWD, and buffer",
		LongHelp: `List prints running Emacs servers, by default that of the current
workspace prefix. With -global it lists every running Emacs server,
and with -visible those on the visible workspaces.

The columns are the server name, workspace, process ID, working directory,
and selected buffer.`,
		FlagSet:   fs,
		UsageFunc: commandUsage,
	}
	cmd.Exec = func(ctx context.Context, args []string) error {
		if len(args) > 1 || (*global && *visible) || ((*global || *visible) && len(args) > 0) {
			usageError(fs)
		}
		target := ""
		if len(args) == 1 {
			target = args[0]
		}
		return executeEmacsList(*global, *visible, target)
	}
	return cmd
}

func newEmacsReadCommand() *ffcli.Command {
	fs := flag.NewFlagSet("ws emacs read", flag.ContinueOnError)
	var common commonReadFlags
	var emacs emacsReadFlags
	common.Register(fs)
	emacs.Register(fs)
	cmd := &ffcli.Command{
		Name:       "read",
		ShortUsage: "ws emacs read [-lines n | -full] [-no-redact] [-ansi] [-no-trim] [-region] [-no-header] ([-except target] (-global | -visible) | target...)",
		ShortHelp:  "print text from Emacs buffers or the marked active region",
		LongHelp: `Read prints the text of windows on visible frames across running Emacs
servers, or the marked active region.

With -region, it prints the active marked selection. By default it checks
the Emacs server of the current workspace prefix, falling back to other
visible Emacs servers if no text is marked. With -global, it checks all
running Emacs servers. A line naming the file and lines precedes the text
unless -no-header is given.

Without -region, read prints the text of windows on the visible frames
across running Emacs servers, each after a line naming its buffer. With
-full it prints the entire buffer contents. With -lines n it prints the
last n lines of each buffer. With -global it prints all windows across
all running Emacs servers.

With -except, read leaves out the Emacs server of target (such as .bot).

Target(s) address specific Emacs servers by workspace or server name.

Read redacts secrets via delemark unless -no-redact is given.`,
		FlagSet:   fs,
		UsageFunc: commandUsage,
	}
	cmd.Exec = func(ctx context.Context, args []string) error {
		if err := common.validate(fs, true); err != nil {
			return err
		}
		skip, err := common.resolveEmacsSkip()
		if err != nil {
			return err
		}
		if emacs.region {
			return runEmacsRegion(args, skip, emacs.noHeader, common.noRedact, common.global)
		}
		return runEmacsBuffers(args, skip, common)
	}
	return cmd
}

func runEmacs(args []string) error {
	return runFFCLI(newEmacsCommand(), args)
}

func runEmacsList(args []string) error {
	return runFFCLI(newEmacsListCommand(), args)
}

func runEmacsRead(args []string) error {
	return runFFCLI(newEmacsReadCommand(), args)
}

func executeEmacsList(global, visible bool, target string) error {
	servers := runningEmacsServers()
	if len(servers) == 0 {
		return errors.New("emacs: no running Emacs servers found")
	}

	switch {
	case !global && !visible:
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

	metaExpr := strings.TrimSpace(elispServerMeta)
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
			if visible && !info.visible {
				continue
			}
			list = append(list, info)
		}
	}

	if len(list) == 0 {
		if visible {
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

func runEmacsRegion(targets []string, skip string, noHeader, noRedact, global bool) error {
	expr := fmt.Sprintf(strings.TrimSpace(elispRegionText), map[bool]string{true: "t", false: "nil"}[noHeader])
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()

	if len(targets) > 0 {
		var errs []error
		for _, target := range targets {
			s, err := resolveEmacsTarget(target)
			if err != nil {
				errs = append(errs, err)
				continue
			}
			if s == skip {
				continue
			}
			text, err := emacsEval(ctx, s, expr)
			if err != nil {
				errs = append(errs, err)
				continue
			}
			if text != "" {
				return printEmacsText(text, noRedact)
			}
		}
		if len(errs) > 0 {
			return errors.Join(errs...)
		}
		return errors.New("emacs: No marked text")
	}

	servers := runningEmacsServers()
	if skip != "" {
		servers = slices.DeleteFunc(servers, func(s string) bool { return s == skip })
	}
	var toCheck []string
	if global {
		toCheck = orderEmacsServers(servers)
	} else {
		toCheck = visibleEmacsServers(servers)
	}
	if len(toCheck) == 0 {
		if w, err := contextWorkspace(); err == nil {
			return fmt.Errorf("emacs: no Emacs server %s", fileName(w.prefix()))
		}
		return errors.New("emacs: no running Emacs servers found")
	}

	for _, s := range toCheck {
		text, err := emacsEval(ctx, s, expr)
		if err == nil && text != "" {
			return printEmacsText(text, noRedact)
		}
	}
	return errors.New("emacs: No marked text")
}

func runEmacsBuffers(targets []string, skip string, flags commonReadFlags) error {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()

	servers := runningEmacsServers()
	if len(servers) == 0 {
		if flags.visible {
			return nil
		}
		if len(targets) > 0 {
			return fmt.Errorf("emacs: no running Emacs servers found for %s", strings.Join(targets, ", "))
		}
		return errors.New("emacs: no running Emacs servers found")
	}

	if skip != "" {
		servers = slices.DeleteFunc(servers, func(s string) bool { return s == skip })
	}

	expr := emacsTextExpr(flags.full, flags.lines, false)
	var toQuery []string

	if len(targets) > 0 {
		expr = emacsTextExpr(flags.full, flags.lines, true)
		var errs []error
		for _, target := range targets {
			s, err := resolveEmacsTarget(target)
			if err != nil {
				errs = append(errs, err)
				continue
			}
			if s == skip {
				continue
			}
			if slices.Contains(servers, s) && !slices.Contains(toQuery, s) {
				toQuery = append(toQuery, s)
			} else if !slices.Contains(servers, s) {
				errs = append(errs, fmt.Errorf("emacs: no running Emacs server %s", s))
			}
		}
		if len(toQuery) == 0 && len(errs) > 0 {
			return errors.Join(errs...)
		}
	} else if flags.global {
		expr = emacsTextExpr(flags.full, flags.lines, true)
		toQuery = orderEmacsServers(servers)
	} else { // visible
		toQuery = visibleEmacsServers(servers)
	}

	if len(toQuery) == 0 {
		return nil
	}

	type serverResult struct {
		server string
		text   string
	}
	results := make([]serverResult, len(toQuery))
	var wg sync.WaitGroup

	for i, s := range toQuery {
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

	showHeader := len(active) > 1 || flags.global || len(targets) > 1
	for i, r := range active {
		if i > 0 {
			fmt.Println()
		}
		if showHeader {
			fmt.Printf("==> emacs:%s <==\n", r.server)
		}
		text := r.text
		if !flags.noRedact {
			text = redact(text)
		}
		fmt.Print(text)
		if !strings.HasSuffix(text, "\n") {
			fmt.Println()
		}
	}
	return nil
}

func visibleEmacsServers(servers []string) []string {
	curPrefix := ""
	if w, err := contextWorkspace(); err == nil {
		curPrefix = fileName(w.prefix())
	}
	visibleMap := make(map[string]bool)
	if curPrefix != "" {
		visibleMap[curPrefix] = true
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
					visibleMap[fileName(Workspace(name).prefix())] = true
				}
			}
		}
	}
	ordered := orderEmacsServers(servers)
	return slices.DeleteFunc(ordered, func(s string) bool { return !visibleMap[s] })
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
	dirs := emacsSocketDirs()
	vlog("emacs: scanning socket directories: %v", dirs)
	for _, dir := range dirs {
		entries, err := os.ReadDir(dir)
		if err != nil {
			vlog("emacs: could not read socket directory %s: %v", dir, err)
			continue
		}
		for _, e := range entries {
			name := e.Name()
			if seen[name] {
				continue
			}
			info, err := e.Info()
			if err != nil {
				vlog("emacs: socket %s stat error: %v", filepath.Join(dir, name), err)
				continue
			}
			if info.Mode()&os.ModeSocket != 0 {
				seen[name] = true
				servers = append(servers, name)
			}
		}
	}
	slices.Sort(servers)
	vlog("emacs: found %d running Emacs server(s): %v", len(servers), servers)
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
	vlog("emacs: evaluating expression in server %q", server)
	if _, err := exec.LookPath("emacsclient"); err != nil {
		vlog("emacs: preferred command %q not found in $PATH: %v", "emacsclient", err)
		return "", fmt.Errorf("emacsclient: %w", err)
	}
	expr = fmt.Sprintf(strings.TrimSpace(elispEvalWrapper), expr)
	// --alternate-editor=false keeps emacsclient from starting a daemon
	// when there is no server, as an empty $ALTERNATE_EDITOR would.
	cmd := exec.CommandContext(ctx, "emacsclient", "--alternate-editor=false", "-s", server, "-e", expr)
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		vlog("emacs: emacsclient failed on server %q: %v (stderr: %q)", server, err, strings.TrimSpace(stderr.String()))
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
		vlog("emacs: base64 decode error from server %q: %v", server, err)
		return "", fmt.Errorf("emacsclient: %w", err)
	}
	vlog("emacs: server %q returned %d bytes", server, len(b))
	return string(b), nil
}
