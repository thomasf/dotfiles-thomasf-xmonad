//go:build linux

package main

import (
	"context"
	"flag"
	"fmt"
	"slices"
	"strings"
	"sync"
	"time"

	critty "github.com/go-pa/critty-ipc"
	"github.com/peterbourgon/ff/v3/ffcli"
)

type readSection struct {
	header string
	text   string
}

func newReadCommand() *ffcli.Command {
	fs := flag.NewFlagSet("ws read", flag.ContinueOnError)
	var common commonReadFlags
	var emacsOpts emacsReadFlags
	var srcOpts sourceFlags
	common.Register(fs)
	emacsOpts.Register(fs)
	srcOpts.Register(fs)
	cmd := &ffcli.Command{
		Name:       "read",
		ShortUsage: "ws read [-lines n | -full] [-no-redact] [-ansi] [-no-trim] [-region] [-no-header] [-except target] [-emacs] [-rc] [-global | -visible | target...]",
		ShortHelp:  "read text from Emacs buffers and controllable terminals",
		LongHelp: `Read prints text from Emacs buffers and remote control terminals across
workspaces and screens.

By default, read prints visible sources on the active screens (like
-visible), querying both Emacs and critty terminals.`,
		FlagSet:   fs,
		UsageFunc: commandUsage,
	}
	cmd.Exec = func(ctx context.Context, args []string) error {
		vlog("read: starting read command with args %v", args)
		if err := common.validate(fs, true); err != nil {
			vlog("read: flag validation error: %v", err)
			return err
		}
		opts := common.screenReader()
		skip, err := common.resolveRcSkip()
		if err != nil {
			vlog("read: resolveRcSkip error: %v", err)
			return err
		}
		if skip != "" {
			vlog("read: skipping caller terminal on %q", skip)
		}

		emacs := srcOpts.emacs
		rc := srcOpts.rc
		if !emacs && !rc {
			emacs = true
			rc = true
		}
		vlog("read: targets emacs=%v rc=%v", emacs, rc)

		ctxTimeout, cancel := context.WithTimeout(ctx, 5*time.Second)
		defer cancel()

		var sections []readSection

		// 1. Emacs
		if emacs {
			emacsSections := collectEmacsSections(ctxTimeout, common, emacsOpts.region, emacsOpts.noHeader, args)
			sections = append(sections, emacsSections...)
		}

		// 2. RC Terminals
		if rc {
			rcSections, err := collectRcSections(ctxTimeout, opts, common.visible, common.global, skip, args)
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
			if !common.noRedact {
				text = redact(text)
			}
			fmt.Print(text)
			if !strings.HasSuffix(text, "\n") {
				fmt.Println()
			}
		}
		return nil
	}
	return cmd
}

func runRead(args []string) error {
	return runFFCLI(newReadCommand(), args)
}

func collectEmacsSections(ctx context.Context, flags commonReadFlags, region, noHeader bool, targets []string) []readSection {
	servers := runningEmacsServers()
	if len(servers) == 0 {
		return nil
	}

	expr := emacsTextExpr(flags.full, flags.lines, false)
	if region {
		expr = fmt.Sprintf(strings.TrimSpace(elispRegionText), map[bool]string{true: "t", false: "nil"}[noHeader])
	} else if flags.global || len(targets) > 0 {
		expr = emacsTextExpr(flags.full, flags.lines, true)
	}

	var toQuery []string
	if flags.global {
		toQuery = orderEmacsServers(servers)
	} else if flags.visible {
		toQuery = visibleEmacsServers(servers)
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
			if err != nil {
				vlog("read: emacsEval error on server %q: %v", server, err)
				return
			}
			if strings.TrimSpace(txt) != "" {
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
			if err != nil {
				vlog("read: rc read error on terminal %q: %v", terms[j].name, err)
				continue
			}
			if strings.TrimSpace(screen) != "" {
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
