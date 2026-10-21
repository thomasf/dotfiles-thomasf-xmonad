//go:build linux

package main

import (
	"cmp"
	"errors"
	"fmt"
	"html"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
)

func runPick(args []string) error {
	fs := newFlagSet("pick", "[-go] [-empty] [-p prompt] [-columns n] [-lines n] [-worktrees]", `
Pick lets the user choose a workspace with rofi and prints its name. It
lists the workspaces with windows other than the current one, grouped
by project. It exits with status 1 if the user cancels.

    Ctrl+Enter  take the name as typed, for a new workspace
    Ctrl+Tab    type in the name of the selected workspace, to edit it;
                again on the same name, add a dot if it has none
    Ctrl+s      show or hide the workspaces without windows
    Ctrl+t      show or hide the worktrees of their projects that have
                no workspace yet; listing them takes a moment

With -go, it goes to the workspace instead of printing it, as ws go
does; this way xmonad can start it with spawn, rather than wait for it
while rofi runs and lock up input.
`)
	prompt := fs.String("p", "workspace", "rofi `prompt`")
	goTo := fs.Bool("go", false, "go to the workspace instead of printing its name")
	withWorktrees := fs.Bool("worktrees", false, "also list worktrees without a workspace")
	withEmpty := fs.Bool("empty", false, "also list workspaces without windows")
	columns := fs.Int("columns", 3, "show `n` columns")
	lines := fs.Int("lines", 20, "show `n` rows in each column")
	fs.Parse(args)
	if fs.NArg() > 0 {
		usageError(fs)
	}
	names, cur, err := desktops()
	if err != nil {
		return err
	}
	current := ""
	if cur < len(names) {
		current = names[cur]
	}
	occupied, err := occupiedDesktops()
	if err != nil {
		return err
	}
	var busy []string
	for i, name := range names {
		if occupied[i] {
			busy = append(busy, name)
		}
	}
	// Listing the worktrees takes a while, so it is done only once
	// they are asked for, and then only once.
	var wts []string
	listed := false
	filter := ""
	// dotted is the last name that Ctrl+Tab typed in with a dot added.
	dotted := ""
	for {
		if *withWorktrees && !listed {
			wts, listed = worktreePrefixes(projectsOf(names)), true
			// pickGroups sees only the workspaces shown, so leave out
			// the worktrees of hidden ones here.
			wts = slices.DeleteFunc(wts, func(p string) bool {
				return slices.ContainsFunc(names, func(t string) bool { return workspace(t).prefix() == p })
			})
		}
		shown := busy
		if *withEmpty {
			shown = names
		}
		var shownWts []string
		if *withWorktrees {
			shownWts = wts
		}
		name, err := rofiPickWorkspace(*prompt, *columns, *lines, filter, *withWorktrees, *withEmpty, pickGroups(current, shown, shownWts))
		if t, ok := errors.AsType[toggleWorktrees](err); ok {
			*withWorktrees = !*withWorktrees
			filter = t.filter
			continue
		}
		if t, ok := errors.AsType[toggleEmpty](err); ok {
			*withEmpty = !*withEmpty
			filter = t.filter
			continue
		}
		if c, ok := errors.AsType[completeRow](err); ok {
			// Completing the same name again readies it for a
			// suffix, unless it has one. The first time leaves it
			// as is, as a dot would filter out the name itself.
			again := c.filter == filter
			filter = c.filter
			if again && filter != "" && !strings.Contains(filter, ".") {
				filter += "."
				dotted = filter
			}
			continue
		}
		if err != nil {
			return err
		}
		// Drop the dot again if nothing was typed after it, unless
		// a workspace has that name.
		if name == dotted && !slices.Contains(names, name) {
			name = strings.TrimSuffix(name, ".")
		}
		if *goTo {
			return goWorkspace(name)
		}
		fmt.Println(name)
		return nil
	}
}

// A pickGroup is a project's workspaces in the picker.
type pickGroup struct {
	project string
	entries []pickEntry
}

type pickEntry struct {
	name string
	open bool // whether the workspace exists; else it is a worktree without one
}

// projectsOf returns the projects of workspaces, each once.
func projectsOf(workspaces []string) []string {
	var ps []string
	for _, t := range workspaces {
		if p := workspace(t).project(); p != "" && !slices.Contains(ps, p) {
			ps = append(ps, p)
		}
	}
	return ps
}

// pickGroups groups the workspaces, other than current and the
// scratchpad workspace NSP, and the worktree prefixes that have no
// workspace by project. The project of current comes first, the others
// follow by name. Within a project, the main checkout comes before its
// worktrees, and a workspace without suffix before numbered ones, before
// other suffixes.
func pickGroups(current string, workspaces, worktrees []string) []pickGroup {
	byProject := make(map[string][]pickEntry)
	prefixes := make(map[string]bool)
	for _, t := range workspaces {
		if t == "" || t == current || t == "NSP" {
			continue
		}
		w := workspace(t)
		prefixes[w.prefix()] = true
		byProject[w.project()] = append(byProject[w.project()], pickEntry{t, true})
	}
	prefixes[workspace(current).prefix()] = true
	for _, p := range worktrees {
		if !prefixes[p] {
			w := workspace(p)
			byProject[w.project()] = append(byProject[w.project()], pickEntry{p, false})
		}
	}

	var groups []pickGroup
	for project, entries := range byProject {
		slices.SortFunc(entries, func(a, b pickEntry) int { return compareWorkspaces(workspace(a.name), workspace(b.name)) })
		groups = append(groups, pickGroup{project, entries})
	}
	first := workspace(current).project()
	slices.SortFunc(groups, func(a, b pickGroup) int {
		if (a.project == first) != (b.project == first) {
			if a.project == first {
				return -1
			}
			return 1
		}
		return strings.Compare(a.project, b.project)
	})
	return groups
}

// compareWorkspaces orders workspaces of one project as pickGroups
// describes.
func compareWorkspaces(a, b workspace) int {
	if c := compareBool(a.worktree() != "", b.worktree() != ""); c != 0 {
		return c
	}
	if c := strings.Compare(a.worktree(), b.worktree()); c != 0 {
		return c
	}
	sa, oka := a.suffix()
	sb, okb := b.suffix()
	if c := compareBool(oka, okb); c != 0 {
		return c
	}
	na, errA := strconv.Atoi(sa)
	nb, errB := strconv.Atoi(sb)
	switch {
	case errA == nil && errB == nil:
		return cmp.Compare(na, nb)
	case errA == nil:
		return -1
	case errB == nil:
		return 1
	}
	return strings.Compare(sa, sb)
}

// compareBool orders false before true.
func compareBool(a, b bool) int {
	switch {
	case a == b:
		return 0
	case a:
		return 1
	}
	return -1
}

// worktreePrefixes returns the workspace prefixes of the worktrees of
// projects. It reads the directories from a single ghq list rather than
// from git, to stay fast: the worktrees of a main checkout <repo> are the
// repositories named <repo>@<name> beside it.
func worktreePrefixes(projects []string) []string {
	out, err := exec.Command("ghq", "list", "-p").Output()
	if err != nil {
		return nil
	}
	dirs := strings.Fields(string(out))
	byName := make(map[string][]string)
	for _, d := range dirs {
		byName[filepath.Base(d)] = append(byName[filepath.Base(d)], d)
	}
	var prefixes []string
	for _, project := range projects {
		main, ok := pickMainCheckout(project, byName)
		if !ok {
			continue
		}
		for _, d := range dirs {
			name, ok := strings.CutPrefix(filepath.Base(d), filepath.Base(main)+"@")
			if ok && filepath.Dir(d) == filepath.Dir(main) && validWorktreeName(name) {
				prefixes = append(prefixes, project+"@"+name)
			}
		}
	}
	return prefixes
}

// pickMainCheckout returns the main checkout of project: from its
// stored directory, or else the one ghq repository named project.
func pickMainCheckout(project string, byName map[string][]string) (string, bool) {
	dir, err := storedDir(project)
	if err != nil {
		return "", false
	}
	if dir == "" {
		if len(byName[project]) != 1 {
			return "", false
		}
		return byName[project][0], true
	}
	main, _, err := mainCheckout(dir)
	return main, err == nil
}

// toggleWorktrees is the error rofiPickWorkspace returns when the user
// asks to show or hide the worktrees without a workspace. filter is
// what the user had typed.
type toggleWorktrees struct{ filter string }

func (toggleWorktrees) Error() string { return "toggle worktrees" }

// toggleEmpty is the error rofiPickWorkspace returns when the user asks
// to show or hide the workspaces without windows. filter is what the
// user had typed.
type toggleEmpty struct{ filter string }

func (toggleEmpty) Error() string { return "toggle empty workspaces" }

// completeRow is the error rofiPickWorkspace returns when the user asks
// to type in the name of the selected row. filter is that name, or what
// the user had typed if no row was selected.
type completeRow struct{ filter string }

func (completeRow) Error() string { return "complete row" }

// rofiPickWorkspace lets the user pick one of the workspaces in groups
// with rofi and returns its name. The list has columns of lines rows,
// filled from top to bottom so that groups read downwards; filter, if
// not empty, is typed in already. The user can also enter a name of
// their own with Ctrl+Enter, or show or hide the worktrees without a
// workspace with Ctrl+t, which returns a toggleWorktrees; worktrees says
// whether they are shown. Likewise Ctrl+s returns a toggleEmpty for the
// workspaces without windows; empty says whether they are shown. Ctrl+Tab returns a completeRow with the
// name of the selected row. It returns errFalse if the user cancels.
func rofiPickWorkspace(prompt string, columns, lines int, filter string, worktrees, empty bool, groups []pickGroup) (string, error) {
	rows, names := pickRows(groups, accentColour())
	theme := fmt.Sprintf("listview { columns: %d; lines: %d; flow: vertical; fixed-columns: true; }", columns, lines)
	// What Ctrl+s and Ctrl+t do: + shows, - hides.
	sign := map[bool]string{false: "+", true: "-"}
	args := []string{"-dmenu", "-i", "-matching", "fuzzy", "-markup-rows",
		"-kb-custom-1", "Control+t", "-kb-custom-3", "Control+s",
		// rofi's own Ctrl+Space types in the row as it is shown,
		// markup and indent included, so ws types in its name on
		// Ctrl+Tab instead. Switching modes has no use in dmenu.
		"-kb-row-select", "", "-kb-mode-next", "Shift+Right", "-kb-custom-2", "Control+Tab",
		"-format", "i f", "-p", prompt,
		"-mesg", "C-Ret typed   C-Tab complete   C-s " + sign[empty] + "empty   C-t " + sign[worktrees] + "worktrees"}
	if filter != "" {
		args = append(args, "-filter", filter)
	}
	cmd, err := rofiCommand(min(95, 35+20*columns), 65, theme, args...)
	if err != nil {
		return "", err
	}
	cmd.Stdin = strings.NewReader(strings.Join(rows, "\n"))
	out, err := cmd.Output()
	i, typed, _ := strings.Cut(strings.TrimSuffix(string(out), "\n"), " ")
	n, nerr := strconv.Atoi(i)
	// rofi exits with status 10 for kb-custom-1, 11 for kb-custom-2,
	// and so on.
	if ee, ok := errors.AsType[*exec.ExitError](err); ok {
		switch ee.ExitCode() {
		case 10:
			return "", toggleWorktrees{typed}
		case 11:
			if nerr == nil && n >= 0 && n < len(names) {
				return "", completeRow{names[n]}
			}
			return "", completeRow{typed}
		case 12:
			return "", toggleEmpty{typed}
		}
	}
	if err != nil {
		return "", errFalse
	}
	switch {
	case nerr != nil:
		return "", fmt.Errorf("rofi: unexpected output %q", out)
	case n >= 0 && n < len(names):
		return names[n], nil
	case n == -1 && strings.TrimSpace(typed) != "":
		return strings.TrimSpace(typed), nil
	}
	return "", errFalse
}

// accentColour returns the colour of labels in rofi rows: Selenized
// cyan, for the light or dark rofi colours that darkmode links
// ~/.config/rofi/colors.rasi to. Pango markup cannot use the colours
// of the rofi theme.
func accentColour() string {
	home, err := os.UserHomeDir()
	if err == nil {
		t, err := os.Readlink(filepath.Join(home, ".config", "rofi", "colors.rasi"))
		if err == nil && filepath.Base(t) == "dark.rasi" {
			return "#41c7b9"
		}
	}
	return "#009c8f"
}

// pickRows returns the rofi rows for groups, and for each row the name
// of its workspace. A group header stands for the first workspace of
// its group: rofi cannot keep the cursor off rows, so picking a header
// picks that rather than nothing. Headers match only their project when
// the user filters, so that a filter on the rest of a name leaves the
// workspaces at the top. Worktrees without a workspace are labelled in
// colour.
func pickRows(groups []pickGroup, colour string) (rows, names []string) {
	for _, g := range groups {
		rows = append(rows, "<b>"+html.EscapeString(g.project)+"</b>")
		names = append(names, g.entries[0].name)
		for _, e := range g.entries {
			row := "  " + html.EscapeString(e.name)
			if !e.open {
				row += `  <span foreground="` + colour + `">worktree</span>`
			}
			rows = append(rows, row)
			names = append(names, e.name)
		}
	}
	return rows, names
}
