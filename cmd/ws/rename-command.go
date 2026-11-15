//go:build linux

package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"html"
	"os"
	"slices"
	"strings"

	"github.com/peterbourgon/ff/v3/ffcli"
)

func newRenameCommand() *ffcli.Command {
	fs := flag.NewFlagSet("ws rename", flag.ContinueOnError)
	cmd := &ffcli.Command{
		Name:       "rename",
		ShortUsage: "ws rename [name]",
		ShortHelp:  "rename the current workspace",
		LongHelp: `Rename renames the current workspace, asking with rofi if name is not given,
with a WS_RENAME client message that the xmonad configuration handles.`,
		FlagSet:   fs,
		UsageFunc: commandUsage,
	}
	cmd.Exec = func(ctx context.Context, args []string) error {
		if len(args) > 1 {
			usageError(fs)
		}
		if len(args) == 1 {
			return renameWorkspace(args[0])
		}
		cur, err := currentWorkspace()
		if err != nil {
			return err
		}
		if isTerminal(os.Stdin) {
			name, _ := readPromptLine(fmt.Sprintf("Rename workspace %s to: ", cur))
			if name == "" || name == string(cur) {
				return nil
			}
			return renameWorkspace(name)
		}
		candidates := renameCandidates(string(cur))
		name, err := rofiRename(string(cur), candidates)
		if err != nil {
			if errors.Is(err, errFalse) || errors.Is(err, errCancelled) {
				return nil
			}
			return err
		}
		name = strings.TrimSpace(name)
		if name == "" || name == string(cur) {
			return nil
		}
		return renameWorkspace(name)
	}
	return cmd
}

func runRename(args []string) error {
	return runFFCLI(newRenameCommand(), args)
}

// renameWorkspace asks xmonad to rename the current workspace to name
// with a WS_RENAME client message carrying an atom that names the new workspace.
func renameWorkspace(name string) error {
	return sendWorkspaceMessage("WS_RENAME", name)
}

// renameCandidates returns workspace names and stems suitable for renaming
// the current workspace, with current at the top.
func renameCandidates(current string) []string {
	names, _, err := desktops()
	if err != nil {
		if current != "" {
			return []string{current}
		}
		return nil
	}

	seen := make(map[string]bool)
	var others []string

	addOther := func(s string) {
		s = strings.TrimSpace(s)
		if s == "" || s == "NSP" || s == current || seen[s] {
			return
		}
		seen[s] = true
		others = append(others, s)
	}

	for _, n := range names {
		addOther(n)
		w := Workspace(n)
		if p := w.prefix(); p != "" {
			addOther(p)
		}
		if proj := w.project(); proj != "" {
			addOther(proj)
		}
	}

	// Worktree prefixes for projects of existing workspaces
	for _, wt := range worktreePrefixes(projectsOf(names)) {
		addOther(wt)
	}

	slices.Sort(others)

	if current != "" {
		return append([]string{current}, others...)
	}
	return others
}

func rofiRename(current string, candidates []string) (string, error) {
	mesg := fmt.Sprintf("Renaming: <b>%s</b>   (Tab/C-Tab copy stem, Ret rename)", html.EscapeString(current))
	theme := "listview { lines: 12; }"
	cmd, err := rofiCommand(50, 0, theme,
		"-dmenu", "-i", "-matching", "fuzzy",
		"-p", "rename",
		"-mesg", mesg,
		"-kb-mode-next", "Shift+Right",
		"-kb-element-next", "",
		"-kb-row-select", "Tab,Control+space,Control+Tab",
		"-format", "s",
	)
	if err != nil {
		return "", err
	}
	cmd.Stdin = strings.NewReader(strings.Join(candidates, "\n"))
	out, err := cmd.Output()
	if err != nil {
		return "", errFalse
	}
	return strings.TrimSpace(string(out)), nil
}
