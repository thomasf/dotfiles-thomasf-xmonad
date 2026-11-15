//go:build linux

package main

import (
	"context"
	"fmt"
	"slices"
	"strings"
	"sync"
	"time"

	critty "github.com/go-pa/critty-ipc"
)

type readSection struct {
	header string
	text   string
}

func runRead(args []string) error {
	fs := newFlagSet("read", "[-ansi] [-no-trim] [-noredact] [-lines n | -history] [-except target] [-emacs] [-rc] [-global | -visible | target...]", `
Read prints text from Emacs buffers and remote control terminals across
workspaces and screens.

By default, read prints visible sources on the active screens (like
-visible), querying both Emacs and critty terminals.

    -visible        read sources visible on the active screens (default)
    -global         read sources across all workspaces and daemons
    target...       read sources of the given workspace prefix(es)

Sources (defaults to both when neither is specified):
    -emacs          include Emacs buffers
    -rc             include remote control terminals

Options:
    -except target  leave out terminals on workspaces matching target
                    (such as .bot)
    -lines n        rows of terminal output (default: visible screen rows)
    -history        all of terminal scrollback and screen
    -ansi           keep terminal ANSI color codes
    -no-trim        do not trim empty lines from terminals
    -noredact       do not redact secrets via delemark
`)
	ansi := fs.Bool("ansi", false, "keep terminal ANSI escape codes")
	noTrim := fs.Bool("no-trim", false, "do not trim terminal screens")
	noRedact := fs.Bool("noredact", false, "do not redact secrets via delemark")
	lines := fs.Int("lines", 0, "rows of terminal output to read")
	history := fs.Bool("history", false, "all of terminal scrollback and the screen")
	except := fs.String("except", "", "leave out terminals of target")
	global := fs.Bool("global", false, "read sources across all workspaces")
	visible := fs.Bool("visible", false, "read sources on visible workspaces")
	emacs := fs.Bool("emacs", false, "include Emacs buffers")
	rc := fs.Bool("rc", false, "include remote control terminals")
	fs.Parse(args)

	if *global && *visible || (*global || *visible) && fs.NArg() > 0 {
		usageError(fs)
	}
	if *history && *lines > 0 {
		usageError(fs)
	}

	// Default to visible if neither global nor targets are specified
	if !*global && fs.NArg() == 0 {
		*visible = true
	}

	// If neither source is explicitly chosen, default to both Emacs and RC terminals
	if !*emacs && !*rc {
		*emacs = true
		*rc = true
	}

	opts := screenReader{
		ReadOptions: critty.ReadOptions{
			Lines: *lines,
			ANSI:  *ansi,
		},
		trim: !*noTrim,
	}
	if *history {
		opts.Lines = -1
	}

	var skip string
	if *except != "" {
		target, err := resolveRcTarget(*except)
		if err != nil {
			return err
		}
		skip, _ = splitRcName(target)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	var sections []readSection

	// 1. Emacs
	if *emacs {
		emacsSections := collectEmacsSections(ctx, *visible, *global, fs.Args())
		sections = append(sections, emacsSections...)
	}

	// 2. RC Terminals
	if *rc {
		rcSections, err := collectRcSections(ctx, opts, *visible, *global, skip, fs.Args())
		if err == nil {
			sections = append(sections, rcSections...)
		}
	}

	// Print all sections
	for i, s := range sections {
		if i > 0 {
			fmt.Println()
		}
		fmt.Printf("==> %s <==\n", s.header)
		text := s.text
		if !*noRedact {
			text = redact(text)
		}
		fmt.Print(text)
		if !strings.HasSuffix(text, "\n") {
			fmt.Println()
		}
	}

	return nil
}

func collectEmacsSections(ctx context.Context, visible, global bool, targets []string) []readSection {
	servers := runningEmacsServers()
	if len(servers) == 0 {
		return nil
	}

	expr := "(my-visible-text)"
	if global {
		expr = emacsAllFramesExpr
	}

	var toQuery []string
	if global || visible {
		toQuery = orderEmacsServers(servers)
	} else {
		for _, target := range targets {
			resolved, err := resolveEmacsTarget(target)
			if err == nil && slices.Contains(servers, resolved) && !slices.Contains(toQuery, resolved) {
				toQuery = append(toQuery, resolved)
			}
		}
	}

	type result struct {
		server string
		text   string
	}
	results := make([]result, len(toQuery))
	var wg sync.WaitGroup
	for i, s := range toQuery {
		wg.Add(1)
		go func(idx int, server string) {
			defer wg.Done()
			txt, err := emacsEval(ctx, server, expr)
			if err == nil && strings.TrimSpace(txt) != "" {
				results[idx] = result{server: server, text: txt}
			}
		}(i, s)
	}
	wg.Wait()

	var sections []readSection
	for _, r := range results {
		if r.text != "" {
			sections = append(sections, readSection{
				header: fmt.Sprintf("emacs:%s", r.server),
				text:   r.text,
			})
		}
	}
	return sections
}

func collectRcSections(ctx context.Context, r screenReader, visible, global bool, skip string, targets []string) ([]readSection, error) {
	terms, err := rcTerminals()
	if err != nil {
		return nil, err
	}

	var sections []readSection

	switch {
	case visible:
		wins, err := visibleTerminalWindows()
		if err != nil {
			return nil, err
		}
		wins = slices.DeleteFunc(wins, func(w Window) bool { return skip != "" && fileName(w.workspace) == skip })
		for _, w := range wins {
			j := slices.IndexFunc(terms, func(t rcTerminal) bool { return t.pid == w.pid })
			if j < 0 {
				continue
			}
			screen, err := r.read(ctx, terms[j].name, critty.Window(w.id))
			if err == nil && strings.TrimSpace(screen) != "" {
				sections = append(sections, readSection{
					header: fmt.Sprintf("term:%s on %s", terms[j].name, w.workspace),
					text:   screen,
				})
			}
		}

	case global:
		terms = slices.DeleteFunc(terms, func(t rcTerminal) bool { return skip != "" && t.base == skip })
		for _, t := range terms {
			screen, err := r.read(ctx, t.name, 0)
			if err == nil && strings.TrimSpace(screen) != "" {
				sections = append(sections, readSection{
					header: fmt.Sprintf("term:%s", t.name),
					text:   screen,
				})
			}
		}

	default:
		var read []rcTerminal
		for _, target := range targets {
			t, err := rcTargets(terms, target, true)
			if err != nil {
				continue
			}
			for _, item := range t {
				if !slices.ContainsFunc(read, func(r rcTerminal) bool { return r.name == item.name }) {
					read = append(read, item)
				}
			}
		}
		for _, t := range read {
			screen, err := r.read(ctx, t.name, 0)
			if err == nil && strings.TrimSpace(screen) != "" {
				sections = append(sections, readSection{
					header: fmt.Sprintf("term:%s", t.name),
					text:   screen,
				})
			}
		}
	}

	return sections, nil
}
