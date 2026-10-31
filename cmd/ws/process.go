//go:build linux

package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

type procNode struct {
	pid     int
	ppid    int
	comm    string
	cmdline []string
	exe     string
	cwd     string
}

type procTree struct {
	children map[int][]int
	nodes    map[int]*procNode
}

// readProcTree scans /proc to build a snapshot of running processes and parent-child links.
func readProcTree() *procTree {
	entries, err := os.ReadDir("/proc")
	if err != nil {
		return &procTree{
			children: make(map[int][]int),
			nodes:    make(map[int]*procNode),
		}
	}
	tree := &procTree{
		children: make(map[int][]int, len(entries)),
		nodes:    make(map[int]*procNode, len(entries)),
	}
	for _, entry := range entries {
		if !entry.IsDir() {
			continue
		}
		name := entry.Name()
		if len(name) == 0 || name[0] < '0' || name[0] > '9' {
			continue
		}
		pid, err := strconv.Atoi(name)
		if err != nil {
			continue
		}
		statData, err := os.ReadFile("/proc/" + name + "/stat")
		if err != nil {
			continue
		}
		s := string(statData)
		lparen := strings.IndexByte(s, '(')
		rparen := strings.LastIndexByte(s, ')')
		if lparen < 0 || rparen < 0 || rparen <= lparen {
			continue
		}
		comm := s[lparen+1 : rparen]
		rest := strings.TrimSpace(s[rparen+1:])
		fields := strings.Fields(rest)
		if len(fields) < 2 {
			continue
		}
		ppid, err := strconv.Atoi(fields[1])
		if err != nil {
			continue
		}
		node := &procNode{
			pid:  pid,
			ppid: ppid,
			comm: comm,
		}
		tree.nodes[pid] = node
		tree.children[ppid] = append(tree.children[ppid], pid)
	}
	return tree
}

// descendants returns all descendant nodes of rootPid in the process tree.
func (t *procTree) descendants(rootPid int) []*procNode {
	if rootPid <= 0 {
		return nil
	}
	var result []*procNode
	queue := []int{rootPid}
	visited := make(map[int]bool)
	visited[rootPid] = true

	for len(queue) > 0 {
		curr := queue[0]
		queue = queue[1:]
		for _, child := range t.children[curr] {
			if !visited[child] {
				visited[child] = true
				if node, ok := t.nodes[child]; ok {
					result = append(result, node)
					queue = append(queue, child)
				}
			}
		}
	}
	return result
}

func (n *procNode) loadDetails() {
	if n.cmdline != nil {
		return
	}
	pidStr := strconv.Itoa(n.pid)
	if data, err := os.ReadFile("/proc/" + pidStr + "/cmdline"); err == nil && len(data) > 0 {
		raw := bytes.Split(data, []byte{0})
		for _, b := range raw {
			if len(b) > 0 {
				n.cmdline = append(n.cmdline, string(b))
			}
		}
	}
	if target, err := os.Readlink("/proc/" + pidStr + "/exe"); err == nil {
		n.exe = target
	}
	if target, err := os.Readlink("/proc/" + pidStr + "/cwd"); err == nil {
		n.cwd = target
	}
}

var commonShells = map[string]bool{
	"bash": true,
	"zsh":  true,
	"fish": true,
	"sh":   true,
	"dash": true,
	"csh":  true,
	"tcsh": true,
}

var terminalComms = map[string]bool{
	"alacritty":       true,
	"kitty":           true,
	"wezterm":         true,
	"wezterm-gui":     true,
	"urxvt":           true,
	"urxvtd":          true,
	"xterm":           true,
	"st":              true,
	"foot":            true,
	"gnome-terminal":  true,
	"gnome-terminal-": true,
	"konsole":         true,
	"xfce4-terminal":  true,
	"termite":         true,
	"tilix":           true,
	"terminator":      true,
	"rxvt":            true,
	"ghostty":         true,
	"term":            true,
	"scratchpad":      true,
}

// processInfo extracts all process names, binaries, arguments from rootPid and all
// its descendants, and identifies the active foreground command and cwd.
func (t *procTree) processInfo(rootPid int) (searchTerms []string, activeCmd string, cwd string) {
	if rootPid <= 0 {
		return nil, "", ""
	}
	desc := t.descendants(rootPid)
	seen := make(map[string]bool)
	addTerm := func(s string) {
		s = strings.TrimSpace(s)
		if s == "" || len(s) > 100 || seen[s] {
			return
		}
		seen[s] = true
		searchTerms = append(searchTerms, s)
	}

	validCwd := func(p string) bool {
		return p != "" && p != "/" && !strings.HasPrefix(p, "/proc")
	}

	isTerminal := false
	if root, ok := t.nodes[rootPid]; ok {
		root.loadDetails()
		addTerm(root.comm)
		if root.exe != "" {
			addTerm(filepath.Base(root.exe))
		}
		comm := strings.ToLower(root.comm)
		exeBase := strings.ToLower(filepath.Base(root.exe))
		isTerminal = terminalComms[comm] || terminalComms[exeBase]

		if validCwd(root.cwd) {
			cwd = root.cwd
		}
	}

	if isTerminal {
		// For terminals, find the interactive shell child (or subshell chain).
		// Never let background daemons, language servers, or MCP workers overwrite the shell cwd.
		var shellNode *procNode
		for _, childPid := range t.children[rootPid] {
			if child, ok := t.nodes[childPid]; ok {
				child.loadDetails()
				if commonShells[child.comm] {
					shellNode = child
					break
				}
			}
		}
		for shellNode != nil {
			if validCwd(shellNode.cwd) {
				cwd = shellNode.cwd
			}
			var nextShell *procNode
			for _, grandPid := range t.children[shellNode.pid] {
				if grand, ok := t.nodes[grandPid]; ok {
					grand.loadDetails()
					if commonShells[grand.comm] {
						nextShell = grand
						break
					}
				}
			}
			shellNode = nextShell
		}
		// If no shell child found (e.g. terminal started with -e command), use first immediate child's cwd
		if cwd == "" {
			for _, childPid := range t.children[rootPid] {
				if child, ok := t.nodes[childPid]; ok {
					child.loadDetails()
					if validCwd(child.cwd) {
						cwd = child.cwd
						break
					}
				}
			}
		}
	}

	// Index only the window's resolved working directory.
	if validCwd(cwd) {
		addTerm(cwd)
		addTerm(filepath.Base(cwd))
		for _, part := range strings.Split(cwd, "/") {
			if part != "" && part != "home" && part != "src" {
				addTerm(part)
			}
		}
	}

	var candidates []string
	for _, n := range desc {
		n.loadDetails()
		addTerm(n.comm)
		if n.exe != "" {
			addTerm(filepath.Base(n.exe))
		}
		var candidate string
		for i, arg := range n.cmdline {
			base := filepath.Base(arg)
			addTerm(base)
			if !strings.HasPrefix(arg, "-") && len(arg) < 60 {
				addTerm(arg)
			}
			if candidate == "" && !strings.HasPrefix(arg, "-") {
				if i == 0 {
					candidate = base
				} else if (n.comm == "node" || n.comm == "python" || n.comm == "python3" ||
					n.comm == "ruby" || n.comm == "perl" || commonShells[n.comm]) &&
					!strings.Contains(base, "python") && !strings.Contains(base, "node") {
					candidate = base
				}
			}
		}
		if candidate != "" && !commonShells[candidate] {
			candidates = append(candidates, candidate)
		} else if !commonShells[n.comm] {
			candidates = append(candidates, n.comm)
		}
	}

	if len(candidates) > 0 {
		activeCmd = candidates[len(candidates)-1]
	}
	return searchTerms, activeCmd, cwd
}
