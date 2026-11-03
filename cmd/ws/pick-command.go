//go:build linux

package main

import (
	"cmp"
	"errors"
	"flag"
	"fmt"
	"html"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
)

func runPick(args []string) error {
	fs := newFlagSet("pick", "[-go] [-shift] [-empty] [-p prompt] [-columns n] [-lines n] [-worktrees]", `
Pick lets the user choose a workspace with rofi and prints its name. It
lists the workspaces with windows or on another screen, and the
current one, grouped by project; those on screen are in colour, and
the current workspace is labelled current. It exits with status 1 if
the user cancels.

    Ctrl+Enter  take the name as typed, for a new workspace
    Ctrl+Tab    type in the name of the selected workspace, to edit it;
                again on the same name, add a dot if it has none; after
                <project>@, choose a branch of the project's repository
                and go to its worktree, created if need be; Ctrl+r
                there fetches the remotes
    Ctrl+s      show or hide the workspaces without windows
    Ctrl+t      show or hide the worktrees of their projects that have
                no workspace yet; listing them takes a moment
    Ctrl+f      fill all screens with workspaces from the selected prefix

With -go, it goes to the workspace instead of printing it, as ws go
does. With -shift, it moves the focused window to the workspace instead
of printing its name, as ws shift does; this way xmonad can start it
with spawn, rather than wait for it while rofi runs and lock up input.
`)
	prompt := fs.String("p", "workspace", "rofi `prompt`")
	goTo := fs.Bool("go", false, "go to the workspace instead of printing its name")
	shiftTo := fs.Bool("shift", false, "move the focused window to the workspace instead of printing its name")
	withWorktrees := fs.Bool("worktrees", false, "also list worktrees without a workspace")
	withEmpty := fs.Bool("empty", false, "also list workspaces without windows")
	columns := fs.Int("columns", 3, "show `n` columns")
	lines := fs.Int("lines", 20, "show `n` rows in each column")
	fs.Parse(args)
	checkArgs(fs, 0, 0)
	hasPrompt := false
	fs.Visit(func(f *flag.Flag) {
		if f.Name == "p" {
			hasPrompt = true
		}
	})
	if *shiftTo && !hasPrompt {
		*prompt = "move to"
	}
	X, err := openX()
	if err != nil {
		return err
	}
	defer X.close()

	names, cur, err := X.desktops()
	if err != nil {
		return err
	}
	current := ""
	if cur < len(names) {
		current = names[cur]
	}
	occupied, err := X.occupiedDesktops()
	if err != nil {
		return err
	}
	onScreen := X.visibleDesktops()
	var busy []string
	visible := make(map[string]bool)
	hasWindows := make(map[string]bool)
	for i, name := range names {
		if occupied[i] || onScreen[i] || i == cur {
			busy = append(busy, name)
		}
		visible[name] = onScreen[i]
		hasWindows[name] = occupied[i]
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
				return slices.ContainsFunc(names, func(t string) bool { return Workspace(t).prefix() == p })
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
		picked, err := rofiPickWorkspaces(*prompt, *columns, *lines, filter, *withWorktrees, *withEmpty, pickGroups(current, shown, visible, hasWindows, shownWts))
		if t, ok := errors.AsType[ToggleWorktrees](err); ok {
			*withWorktrees = !*withWorktrees
			filter = t.filter
			continue
		}
		if t, ok := errors.AsType[ToggleEmpty](err); ok {
			*withEmpty = !*withEmpty
			filter = t.filter
			continue
		}
		if b, ok := errors.AsType[ChooseBranch](err); ok {
			branchWs, bErr := pickBranchWorktree(b.project, *columns, *lines)
			if errors.Is(bErr, errCancelled) {
				filter = b.project + "@"
				continue
			}
			if bErr != nil {
				return bErr
			}
			picked = []string{branchWs}
		}
		if c, ok := errors.AsType[CompleteRow](err); ok {
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
		if f, ok := errors.AsType[FillScreens](err); ok {
			name := f.workspace
			if name == dotted && !slices.Contains(names, name) {
				name = strings.TrimSuffix(name, ".")
			}
			return X.sendWorkspaceMessage("WS_FILL", name)
		}
		if err != nil {
			return err
		}
		// Drop the dot again if nothing was typed after it, unless
		// a workspace has that name.
		for i, name := range picked {
			if name == dotted && !slices.Contains(names, name) {
				picked[i] = strings.TrimSuffix(name, ".")
			}
		}
		if *goTo {
			if len(picked) == 1 {
				return X.sendWorkspaceMessage("WS_GO", picked[0])
			}
			return X.sendWorkspaceMessage("WS_GO_MANY", strings.Join(picked, " "))
		}
		if *shiftTo {
			return X.sendWorkspaceMessage("WS_SHIFT", picked[0])
		}
		for _, name := range picked {
			fmt.Println(name)
		}
		return nil
	}
}

// A PickGroup is a project's workspaces in the picker.
type PickGroup struct {
	project string
	entries []PickEntry
}

type PickEntry struct {
	name     string
	open     bool // whether the workspace exists; else it is a worktree without one
	visible  bool // whether a screen shows it
	current  bool // whether this is the current workspace
	occupied bool // whether it has windows
}

// projectsOf returns the projects of workspaces, each once.
func projectsOf(workspaces []string) []string {
	var ps []string
	for _, t := range workspaces {
		if p := Workspace(t).project(); p != "" && !slices.Contains(ps, p) {
			ps = append(ps, p)
		}
	}
	return ps
}

// pickGroups groups the workspaces, other than the scratchpad workspace NSP,
// and the worktree prefixes that have no workspace by project. visible holds
// the workspaces on other screens. The project of current comes first, the others
// follow by name. Within a project, the main checkout comes before its
// worktrees, and a workspace without suffix before numbered ones, before
// other suffixes.
func pickGroups(current string, workspaces []string, visible map[string]bool, occupied map[string]bool, worktrees []string) []PickGroup {
	byProject := make(map[string][]PickEntry)
	prefixes := make(map[string]bool)
	for _, t := range workspaces {
		if t == "" || t == "NSP" {
			continue
		}
		w := Workspace(t)
		prefixes[w.prefix()] = true
		byProject[w.project()] = append(byProject[w.project()], PickEntry{
			name:     t,
			open:     true,
			visible:  visible[t] || t == current,
			current:  t == current,
			occupied: occupied[t],
		})
	}
	prefixes[Workspace(current).prefix()] = true
	for _, p := range worktrees {
		if !prefixes[p] {
			w := Workspace(p)
			byProject[w.project()] = append(byProject[w.project()], PickEntry{
				name:     p,
				open:     false,
				visible:  false,
				current:  false,
				occupied: false,
			})
		}
	}

	var groups []PickGroup
	for project, entries := range byProject {
		slices.SortFunc(entries, func(a, b PickEntry) int { return compareWorkspaces(Workspace(a.name), Workspace(b.name)) })
		groups = append(groups, PickGroup{project, entries})
	}
	first := Workspace(current).project()
	slices.SortFunc(groups, func(a, b PickGroup) int {
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
func compareWorkspaces(a, b Workspace) int {
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
	dirs, err := ghqList()
	if err != nil {
		return nil
	}
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
			if name, ok := main.parseWorktreeDir(d); ok {
				prefixes = append(prefixes, project+"@"+name)
			}
		}
	}
	return prefixes
}

// pickMainCheckout returns the main checkout of project: from its
// stored directory, or else the one ghq repository named project.
func pickMainCheckout(project string, byName map[string][]string) (Repository, bool) {
	dir, err := storedDir(project)
	if err != nil {
		return "", false
	}
	if dir == "" {
		if len(byName[project]) != 1 {
			return "", false
		}
		return Repository(byName[project][0]), true
	}
	main, _, err := mainCheckout(dir)
	return main, err == nil
}

// ToggleWorktrees is the error rofiPickWorkspace returns when the user
// asks to show or hide the worktrees without a workspace. filter is
// what the user had typed.
type ToggleWorktrees struct{ filter string }

func (ToggleWorktrees) Error() string { return "toggle worktrees" }

// ToggleEmpty is the error rofiPickWorkspace returns when the user asks
// to show or hide the workspaces without windows. filter is what the
// user had typed.
type ToggleEmpty struct{ filter string }

func (ToggleEmpty) Error() string { return "toggle empty workspaces" }

// CompleteRow is the error rofiPickWorkspace returns when the user asks
// to type in the name of the selected row. filter is that name, or what
// the user had typed if no row was selected.
type CompleteRow struct{ filter string }

func (CompleteRow) Error() string { return "complete row" }

// ChooseBranch is the error rofiPickWorkspace returns when the user asks
// to choose a branch of project, having typed <project>@.
type ChooseBranch struct{ project string }

func (ChooseBranch) Error() string { return "choose branch" }

// FillScreens is the error rofiPickWorkspace returns when the user asks
// to fill all screens with workspaces from the selected prefix.
type FillScreens struct{ workspace string }

func (FillScreens) Error() string { return "fill screens" }

// rofiPickWorkspace lets the user pick one of the workspaces in groups
// with rofi and returns its name. The list has columns of lines rows,
// filled from top to bottom so that groups read downwards; filter, if
// not empty, is typed in already. The user can also enter a name of
// their own with Ctrl+Enter, or show or hide the worktrees without a
// workspace with Ctrl+t, which returns a ToggleWorktrees; worktrees says
// whether they are shown. Likewise Ctrl+s returns a ToggleEmpty for the
// workspaces without windows; empty says whether they are shown.
// Ctrl+Tab returns a CompleteRow with the name of the selected row, or
// a ChooseBranch if the user typed <project>@. It returns errFalse if
// the user cancels.
// rofiPickWorkspaces lets the user pick one or more of the workspaces in groups
// with rofi. It returns their names. The user may toggle showing the
// git worktree prefixes that don't yet have a workspace with Ctrl+t,
// which returns a ToggleWorktrees; worktrees says whether they are shown.
// Likewise Ctrl+s returns a ToggleEmpty for the workspaces without windows;
// empty says whether they are shown. Ctrl+Tab returns a CompleteRow with
// the name of the selected row, or a ChooseBranch if the user typed <project>@.
// It returns errFalse if the user cancels.
func rofiPickWorkspaces(prompt string, columns, lines int, filter string, worktrees, empty bool, groups []PickGroup) ([]string, error) {
	rows, names := pickRows(groups, accentColor(), visibleColor())
	theme := fmt.Sprintf("listview { columns: %d; lines: %d; flow: vertical; fixed-columns: true; }", columns, lines)
	// What Ctrl+s and Ctrl+t do: + shows, - hides.
	sign := map[bool]string{false: "+", true: "-"}
	args := []string{"-dmenu", "-i", "-matching", "fuzzy", "-markup-rows", "-multi-select",
		"-ballot-selected-str", pangoColor("* ", selectedColor()), "-ballot-unselected-str", "  ",
		"-kb-custom-1", "Control+t", "-kb-custom-3", "Control+s",
		"-kb-move-char-forward", "Right", "-kb-custom-4", "Control+f",
		"-kb-row-select", "", "-kb-accept-alt", "Control+space,Shift+space,Shift+Return", "-kb-mode-next", "Shift+Right", "-kb-custom-2", "Control+Tab",
		"-format", "i f", "-p", prompt,
		"-mesg", "C-Space mark   C-Ret typed   C-Tab complete   C-f fill   C-s " + sign[empty] + "empty   C-t " + sign[worktrees] + "worktrees"}
	if filter != "" {
		args = append(args, "-filter", filter)
	}
	cmd, err := rofiCommand(pickWidth(columns), pickHeight, theme, args...)
	if err != nil {
		return nil, err
	}
	cmd.Stdin = strings.NewReader(strings.Join(rows, "\n"))
	out, err := cmd.Output()
	indices, typed, nerr := parseRofiIndices(out)
	if key, ok := rofiCustomKey(err); ok {
		firstIdx := -1
		if len(indices) > 0 {
			firstIdx = indices[0]
		}
		switch key {
		case 1:
			return nil, ToggleWorktrees{typed}
		case 2:
			if p, ok := strings.CutSuffix(typed, "@"); ok && p != "" && !strings.ContainsAny(p, ".@ ") {
				return nil, ChooseBranch{p}
			}
			if firstIdx >= 0 && firstIdx < len(names) {
				return nil, CompleteRow{names[firstIdx]}
			}
			return nil, CompleteRow{typed}
		case 3:
			return nil, ToggleEmpty{typed}
		case 4:
			if firstIdx >= 0 && firstIdx < len(names) {
				return nil, FillScreens{names[firstIdx]}
			}
			return nil, FillScreens{typed}
		}
	}
	if err != nil {
		return nil, errFalse
	}
	if nerr != nil {
		return nil, fmt.Errorf("rofi: unexpected output %q", out)
	}
	var selected []string
	for _, idx := range indices {
		if idx >= 0 && idx < len(names) {
			selected = append(selected, names[idx])
		} else if idx == -1 && strings.TrimSpace(typed) != "" {
			selected = append(selected, strings.TrimSpace(typed))
		}
	}
	if len(selected) == 0 && strings.TrimSpace(typed) != "" {
		selected = append(selected, strings.TrimSpace(typed))
	}
	if len(selected) == 0 {
		return nil, errFalse
	}
	return selected, nil
}

func rofiPickWorkspace(prompt string, columns, lines int, filter string, worktrees, empty bool, groups []PickGroup) (string, error) {
	ws, err := rofiPickWorkspaces(prompt, columns, lines, filter, worktrees, empty, groups)
	if err != nil || len(ws) == 0 {
		return "", err
	}
	return ws[0], nil
}

// pickWidth is how wide the picker is, in percent of the xmonad screen,
// with columns columns; pickHeight is how high.
func pickWidth(columns int) int { return min(95, 35+20*columns) }

const pickHeight = 65

// pickRows returns the rofi rows for groups, and for each row the name
// of its workspace. A group header stands for the first workspace of
// its group: rofi cannot keep the cursor off rows, so picking a header
// picks that rather than nothing. Headers match only their project when
// the user filters, so that a filter on the rest of a name leaves the
// workspaces at the top. Worktrees without a workspace are labelled in
// colour, workspaces on other screens are in visible, the current
// workspace is in visible and labelled current, and empty workspaces
// are dimmed.
func pickRows(groups []PickGroup, colour, visible string) (rows, names []string) {
	for _, g := range groups {
		rows = append(rows, pangoBold(g.project))
		names = append(names, g.entries[0].name)
		for _, e := range g.entries {
			var nameHtml string
			switch {
			case e.visible && e.occupied:
				nameHtml = pangoColor(e.name, visible)
			case e.visible && !e.occupied:
				nameHtml = pangoColorDim(e.name, visible, 60)
			case !e.visible && !e.occupied && e.open:
				nameHtml = pangoDim(e.name, 60)
			default:
				nameHtml = html.EscapeString(e.name)
			}
			row := "  " + nameHtml
			if e.current {
				row += "  " + pangoDim("current", 60)
			}
			if !e.open {
				row += "  " + pangoColor("worktree", colour)
			}
			rows = append(rows, row)
			names = append(names, e.name)
		}
	}
	return rows, names
}
