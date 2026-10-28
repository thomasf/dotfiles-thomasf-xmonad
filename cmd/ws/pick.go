//go:build linux

package main

import (
	"cmp"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"html"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"sync"
	"unicode/utf8"
)

func runPick(args []string) error {
	fs := newFlagSet("pick", "[-go] [-shift] [-empty] [-p prompt] [-columns n] [-lines n] [-worktrees]", `
Pick lets the user choose a workspace with rofi and prints its name. It
lists the workspaces with windows or on another screen, other than the
current one, grouped by project; those on other screens are in colour.
It exits with status 1 if the user cancels.

    Ctrl+Enter  take the name as typed, for a new workspace
    Ctrl+Tab    type in the name of the selected workspace, to edit it;
                again on the same name, add a dot if it has none; after
                <project>@, choose a branch of the project's repository
                and go to its worktree, created if need be; Ctrl+r
                there fetches the remotes
    Ctrl+s      show or hide the workspaces without windows
    Ctrl+t      show or hide the worktrees of their projects that have
                no workspace yet; listing them takes a moment

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
	if fs.NArg() > 0 {
		usageError(fs)
	}
	hasPrompt := false
	fs.Visit(func(f *flag.Flag) {
		if f.Name == "p" {
			hasPrompt = true
		}
	})
	if *shiftTo && !hasPrompt {
		*prompt = "move to"
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
	onScreen := visibleDesktops()
	var busy []string
	visible := make(map[string]bool)
	for i, name := range names {
		if occupied[i] || onScreen[i] {
			busy = append(busy, name)
		}
		visible[name] = onScreen[i]
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
		name, err := rofiPickWorkspace(*prompt, *columns, *lines, filter, *withWorktrees, *withEmpty, pickGroups(current, shown, visible, shownWts))
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
		if b, ok := errors.AsType[chooseBranch](err); ok {
			name, err = pickBranchWorktree(b.project, *columns, *lines)
			if errors.Is(err, errCancelled) {
				filter = b.project + "@"
				continue
			}
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
		if *shiftTo {
			return shiftWorkspace(name)
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
	name    string
	open    bool // whether the workspace exists; else it is a worktree without one
	visible bool // whether a screen other than the current one shows it
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
// workspace by project. visible holds the workspaces on other screens. The project of current comes first, the others
// follow by name. Within a project, the main checkout comes before its
// worktrees, and a workspace without suffix before numbered ones, before
// other suffixes.
func pickGroups(current string, workspaces []string, visible map[string]bool, worktrees []string) []pickGroup {
	byProject := make(map[string][]pickEntry)
	prefixes := make(map[string]bool)
	for _, t := range workspaces {
		if t == "" || t == current || t == "NSP" {
			continue
		}
		w := workspace(t)
		prefixes[w.prefix()] = true
		byProject[w.project()] = append(byProject[w.project()], pickEntry{t, true, visible[t]})
	}
	prefixes[workspace(current).prefix()] = true
	for _, p := range worktrees {
		if !prefixes[p] {
			w := workspace(p)
			byProject[w.project()] = append(byProject[w.project()], pickEntry{p, false, false})
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

// chooseBranch is the error rofiPickWorkspace returns when the user asks
// to choose a branch of project, having typed <project>@.
type chooseBranch struct{ project string }

func (chooseBranch) Error() string { return "choose branch" }

// rofiPickWorkspace lets the user pick one of the workspaces in groups
// with rofi and returns its name. The list has columns of lines rows,
// filled from top to bottom so that groups read downwards; filter, if
// not empty, is typed in already. The user can also enter a name of
// their own with Ctrl+Enter, or show or hide the worktrees without a
// workspace with Ctrl+t, which returns a toggleWorktrees; worktrees says
// whether they are shown. Likewise Ctrl+s returns a toggleEmpty for the
// workspaces without windows; empty says whether they are shown.
// Ctrl+Tab returns a completeRow with the name of the selected row, or
// a chooseBranch if the user typed <project>@. It returns errFalse if
// the user cancels.
func rofiPickWorkspace(prompt string, columns, lines int, filter string, worktrees, empty bool, groups []pickGroup) (string, error) {
	rows, names := pickRows(groups, accentColour(), visibleColour())
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
	cmd, err := rofiCommand(pickWidth(columns), pickHeight, theme, args...)
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
			if p, ok := strings.CutSuffix(typed, "@"); ok && p != "" && !strings.ContainsAny(p, ".@ ") {
				return "", chooseBranch{p}
			}
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

// pickWidth is how wide the picker is, in percent of the xmonad screen,
// with columns columns; pickHeight is how high.
func pickWidth(columns int) int { return min(95, 35+20*columns) }

const pickHeight = 65

// accentColour returns the colour of labels in rofi rows: cyan of the
// theme in use. Pango markup cannot use the colours of the rofi theme.
func accentColour() string { return paletteColour("cyan", "#009c8f") }

// visibleColour returns the colour of workspaces on other screens in
// rofi rows: blue of the theme in use.
func visibleColour() string { return paletteColour("blue", "#0072d4") }

// paletteColour returns the colour name of the theme in use, from the
// palette that dotfiles writes to ~/.config/theme/palette.json, or def,
// Selenized light, if the palette cannot be read.
func paletteColour(name, def string) string {
	if c := palette()[name]; c != "" {
		return c
	}
	return def
}

// palette returns the colours of ~/.config/theme/palette.json, or nil.
var palette = sync.OnceValue(func() map[string]string {
	home, err := os.UserHomeDir()
	if err != nil {
		return nil
	}
	data, err := os.ReadFile(filepath.Join(home, ".config", "theme", "palette.json"))
	if err != nil {
		return nil
	}
	var p struct {
		Colors map[string]string `json:"colors"`
	}
	if err := json.Unmarshal(data, &p); err != nil {
		return nil
	}
	return p.Colors
})

// pickRows returns the rofi rows for groups, and for each row the name
// of its workspace. A group header stands for the first workspace of
// its group: rofi cannot keep the cursor off rows, so picking a header
// picks that rather than nothing. Headers match only their project when
// the user filters, so that a filter on the rest of a name leaves the
// workspaces at the top. Worktrees without a workspace are labelled in
// colour, and workspaces on other screens are in visible.
func pickRows(groups []pickGroup, colour, visible string) (rows, names []string) {
	for _, g := range groups {
		rows = append(rows, "<b>"+html.EscapeString(g.project)+"</b>")
		names = append(names, g.entries[0].name)
		for _, e := range g.entries {
			row := "  " + html.EscapeString(e.name)
			if e.visible {
				row = `  <span foreground="` + visible + `">` + html.EscapeString(e.name) + "</span>"
			}
			if !e.open {
				row += `  <span foreground="` + colour + `">worktree</span>`
			}
			rows = append(rows, row)
			names = append(names, e.name)
		}
	}
	return rows, names
}

// pickBranchWorktree lets the user choose a branch of the repository of
// project with rofi, the last changed first, and returns the workspace
// prefix of its worktree, which it creates if there is none. The user
// can fetch the remotes first. rofi is as large as the picker with
// columns columns of lines rows. It returns errCancelled if the user
// cancels or the repository is not known.
func pickBranchWorktree(project string, columns, lines int) (string, error) {
	main, err := projectCheckout(project, true)
	if err != nil {
		log.Print(err)
		return "", errCancelled
	}
	var infos []branchInfo
	var checkedOut map[string]string // branch to workspace prefix
	i, filter := 0, ""
	for {
		if infos, err = branches(main, "-committerdate"); err != nil {
			return "", err
		}
		wts, err := listWorktrees(project, main)
		if err != nil {
			return "", err
		}
		checkedOut = make(map[string]string)
		for _, wt := range wts {
			checkedOut[wt.branch] = wt.prefix
		}
		i, err = rofiPickBranch(project, columns, lines, filter, branchPickRows(infos, checkedOut, accentColour()))
		if f, ok := errors.AsType[fetchBranches](err); ok {
			filter = f.filter
			fetchRemotes(main, project)
			continue
		}
		if err != nil {
			return "", err
		}
		break
	}
	src := infos[i].branchSource
	if src.remote == "" {
		if prefix, ok := checkedOut[src.branch]; ok {
			return prefix, nil
		}
	}
	name := strings.ReplaceAll(src.branch, "/", "-")
	prefix := project + "@" + name
	dir := worktreePath(main, name)
	_, statErr := os.Stat(dir)
	switch {
	case !validWorktreeName(name):
		err = fmt.Errorf("branch %s cannot name a worktree", src.branch)
	case statErr == nil:
		err = fmt.Errorf("worktree %s exists, on another branch: %s", prefix, dir)
	default:
		err = addWorktree(main, name, src)
	}
	if err != nil {
		notify("", true, "creating worktree "+prefix+" failed: "+err.Error())
		return "", err
	}
	notify("", false, "created "+prefix+": "+src.String())
	return prefix, nil
}

// branchPickRows returns infos as rofi rows in Pango markup, in columns
// that line up, as rofi's font is monospace: when the branch last
// changed and by whom, the branch, with the workspace prefix of the
// worktree that has it checked out, from checkedOut, or the remote it
// is only on in colour, and the subject of its last commit.
func branchPickRows(infos []branchInfo, checkedOut map[string]string, colour string) []string {
	notes := make([]string, len(infos))
	var ageWidth, authorWidth, branchWidth int
	for i, b := range infos {
		if b.remote != "" {
			notes[i], _, _ = strings.Cut(b.remote, "/")
		} else {
			notes[i] = checkedOut[b.branch]
		}
		ageWidth = max(ageWidth, utf8.RuneCountInString(b.age))
		authorWidth = max(authorWidth, utf8.RuneCountInString(b.author))
		branchWidth = max(branchWidth, utf8.RuneCountInString(b.branch)+noteWidth(notes[i]))
	}
	pad := func(s string, w int) string {
		return strings.Repeat(" ", w-utf8.RuneCountInString(s))
	}
	rows := make([]string, len(infos))
	for i, b := range infos {
		row := `<span fgalpha="55%">` + html.EscapeString(b.age) + pad(b.age, ageWidth) + "  " +
			html.EscapeString(b.author) + "</span>" + pad(b.author, authorWidth) + "  " +
			"<b>" + html.EscapeString(b.branch) + "</b>"
		if notes[i] != "" {
			row += ` <span foreground="` + colour + `">` + html.EscapeString(notes[i]) + "</span>"
		}
		row += strings.Repeat(" ", branchWidth-utf8.RuneCountInString(b.branch)-noteWidth(notes[i])) + "  " +
			`<span fgalpha="55%">` + html.EscapeString(b.subject) + "</span>"
		rows[i] = row
	}
	return rows
}

// noteWidth returns how wide note is after a branch in branchPickRows.
func noteWidth(note string) int {
	if note == "" {
		return 0
	}
	return 1 + utf8.RuneCountInString(note)
}

// fetchRemotes fetches the remotes of the repository of main checkout
// main, of project, telling the user in a notification.
func fetchRemotes(main, project string) {
	id := notify("", false, "fetching "+project)
	var errs []string
	for _, r := range sortRemotes(main) {
		if err := fetchRemote(main, r); err != nil {
			errs = append(errs, err.Error())
		}
	}
	if len(errs) > 0 {
		notify(id, true, "fetching "+project+" failed: "+strings.Join(errs, "; "))
		return
	}
	notify(id, false, "fetched "+project)
}

// fetchBranches is the error rofiPickBranch returns when the user asks
// to fetch the remotes. filter is what the user had typed.
type fetchBranches struct{ filter string }

func (fetchBranches) Error() string { return "fetch" }

// rofiPickBranch lets the user choose one of rows, the branches of
// project, with rofi as large as the picker with columns columns of
// lines rows, and returns its index. filter, if not empty, is typed in
// already. Ctrl+r returns a fetchBranches. It returns errCancelled if
// the user cancels.
func rofiPickBranch(project string, columns, lines int, filter string, rows []string) (int, error) {
	theme := fmt.Sprintf("listview { columns: 1; lines: %d; }", lines)
	args := []string{"-dmenu", "-i", "-matching", "fuzzy", "-no-custom", "-markup-rows",
		"-kb-custom-1", "Control+r", "-format", "i f", "-p", project + "@",
		"-mesg", "Ret go to its worktree, created if need be   C-r fetch   Esc back"}
	if filter != "" {
		args = append(args, "-filter", filter)
	}
	cmd, err := rofiCommand(pickWidth(columns), pickHeight, theme, args...)
	if err != nil {
		return 0, err
	}
	cmd.Stdin = strings.NewReader(strings.Join(rows, "\n"))
	out, err := cmd.Output()
	i, typed, _ := strings.Cut(strings.TrimSuffix(string(out), "\n"), " ")
	if ee, ok := errors.AsType[*exec.ExitError](err); ok && ee.ExitCode() == 10 {
		return 0, fetchBranches{typed}
	}
	if err != nil {
		return 0, errCancelled
	}
	n, err := strconv.Atoi(i)
	if err != nil || n < 0 || n >= len(rows) {
		return 0, errCancelled
	}
	return n, nil
}
