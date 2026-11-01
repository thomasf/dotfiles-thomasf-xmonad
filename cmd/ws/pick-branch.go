//go:build linux

package main

import (
	"errors"
	"fmt"
	"html"
	"log"
	"strings"
	"unicode/utf8"
)

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
	var infos []BranchInfo
	var checkedOut map[string]string // branch to workspace prefix
	i, filter := 0, ""
	for {
		if infos, err = main.branches("-committerdate"); err != nil {
			return "", err
		}
		wts, err := main.listWorktrees(project)
		if err != nil {
			return "", err
		}
		checkedOut = make(map[string]string)
		for _, wt := range wts {
			checkedOut[wt.branch] = wt.prefix
		}
		i, err = rofiPickBranch(project, columns, lines, filter, branchPickRows(infos, checkedOut, accentColor()))
		if f, ok := errors.AsType[FetchBranches](err); ok {
			filter = f.filter
			fetchRemotes(main, project)
			continue
		}
		if err != nil {
			return "", err
		}
		break
	}
	src := infos[i].BranchSource
	if src.remote == "" {
		if prefix, ok := checkedOut[src.branch]; ok {
			return prefix, nil
		}
	}
	name := strings.ReplaceAll(src.branch, "/", "-")
	prefix := project + "@" + name
	dir := main.worktreePath(name)
	switch {
	case !validWorktreeName(name):
		err = fmt.Errorf("branch %s cannot name a worktree", src.branch)
	case fileExists(dir):
		err = fmt.Errorf("worktree %s exists, on another branch: %s", prefix, dir)
	default:
		err = main.addWorktree(name, src)
	}
	if err != nil {
		notifyError("", "creating worktree "+prefix+" failed: "+err.Error())
		return "", err
	}
	notify("", "created "+prefix+": "+src.String())
	return prefix, nil
}

// branchPickRows returns infos as rofi rows in Pango markup, in columns
// that line up, as rofi's font is monospace: when the branch last
// changed and by whom, the branch, with the workspace prefix of the
// worktree that has it checked out, from checkedOut, or the remote it
// is only on in colour, and the subject of its last commit.
func branchPickRows(infos []BranchInfo, checkedOut map[string]string, colour string) []string {
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
	rows := make([]string, len(infos))
	for i, b := range infos {
		row := `<span fgalpha="55%">` + html.EscapeString(b.age) + padRight(b.age, ageWidth) + "  " +
			html.EscapeString(b.author) + "</span>" + padRight(b.author, authorWidth) + "  " +
			pangoBold(b.branch)
		if notes[i] != "" {
			row += " " + pangoColor(notes[i], colour)
		}
		row += strings.Repeat(" ", branchWidth-utf8.RuneCountInString(b.branch)-noteWidth(notes[i])) + "  " +
			pangoDim(b.subject, 55)
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
func fetchRemotes(main Repository, project string) {
	id := notify("", "fetching "+project)
	var errs []string
	for _, r := range main.sortRemotes() {
		if err := main.fetchRemote(r); err != nil {
			errs = append(errs, err.Error())
		}
	}
	if len(errs) > 0 {
		notifyError(id, "fetching "+project+" failed: "+strings.Join(errs, "; "))
		return
	}
	notify(id, "fetched "+project)
}

// FetchBranches is the error rofiPickBranch returns when the user asks
// to fetch the remotes. filter is what the user had typed.
type FetchBranches struct{ filter string }

func (FetchBranches) Error() string { return "fetch" }

// rofiPickBranch lets the user choose one of rows, the branches of
// project, with rofi as large as the picker with columns columns of
// lines rows, and returns its index. filter, if not empty, is typed in
// already. Ctrl+r returns a FetchBranches. It returns errCancelled if
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
	n, typed, parseErr := parseRofiIndex(out)
	if key, ok := rofiCustomKey(err); ok && key == 1 {
		return 0, FetchBranches{typed}
	}
	if err != nil || parseErr != nil || n < 0 || n >= len(rows) {
		return 0, errCancelled
	}
	return n, nil
}
