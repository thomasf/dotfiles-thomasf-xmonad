//go:build linux

package main

import (
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
	"time"
)

func runHistoryPath() string {
	return xdgCacheFile("run_history")
}

type FrecencyEntry struct {
	count int
	last  time.Time
}

func (e FrecencyEntry) score(now time.Time) float64 {
	if e.count <= 0 {
		return 0
	}
	age := now.Sub(e.last)
	var weight float64
	switch {
	case age < 4*time.Hour:
		weight = 100
	case age < 24*time.Hour:
		weight = 80
	case age < 7*24*time.Hour:
		weight = 60
	case age < 30*24*time.Hour:
		weight = 40
	default:
		weight = 20
	}
	return float64(e.count) * weight
}

type FrecencyHistory struct {
	path    string
	entries map[string]FrecencyEntry
}

func loadFrecency(path string) FrecencyHistory {
	hist := FrecencyHistory{
		path:    path,
		entries: make(map[string]FrecencyEntry),
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return hist
	}
	for line := range strings.SplitSeq(string(data), "\n") {
		fields := strings.Split(line, "\t")
		if len(fields) < 3 {
			continue
		}
		name := fields[0]
		count, err := strconv.Atoi(fields[1])
		if err != nil {
			continue
		}
		ts, err := strconv.ParseInt(fields[2], 10, 64)
		if err != nil {
			continue
		}
		hist.entries[name] = FrecencyEntry{
			count: count,
			last:  time.Unix(ts, 0),
		}
	}
	return hist
}

func (h FrecencyHistory) save() {
	if h.path == "" {
		return
	}
	var b strings.Builder
	for name, entry := range h.entries {
		fmt.Fprintf(&b, "%s\t%d\t%d\n", name, entry.count, entry.last.Unix())
	}
	_ = writeAtomic(h.path, []byte(b.String()), 0o644)
}

func (h FrecencyHistory) record(cmd string) {
	if h.path == "" || cmd == "" {
		return
	}
	entry := h.entries[cmd]
	entry.count++
	entry.last = time.Now()
	h.entries[cmd] = entry
	h.save()
}

func (h FrecencyHistory) sort(bins []string) {
	if len(h.entries) == 0 {
		return
	}
	now := time.Now()
	scores := make(map[string]float64, len(h.entries))
	for name, entry := range h.entries {
		scores[name] = entry.score(now)
	}

	slices.SortStableFunc(bins, func(a, b string) int {
		sa := scores[a]
		sb := scores[b]
		if sa != sb {
			if sb > sa {
				return 1
			}
			return -1
		}
		return strings.Compare(a, b)
	})
}
