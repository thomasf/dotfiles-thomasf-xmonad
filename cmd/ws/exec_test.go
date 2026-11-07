//go:build linux

package main

import (
	"slices"
	"testing"
)

func TestMergeEnviron(t *testing.T) {
	tests := []struct {
		name      string
		base      []string
		overrides map[string]string
		want      []string
	}{
		{
			name:      "nil overrides",
			base:      []string{"A=1", "B=2"},
			overrides: nil,
			want:      []string{"A=1", "B=2"},
		},
		{
			name:      "empty overrides",
			base:      []string{"A=1", "B=2"},
			overrides: map[string]string{},
			want:      []string{"A=1", "B=2"},
		},
		{
			name:      "add new variables",
			base:      []string{"A=1"},
			overrides: map[string]string{"B": "2", "C": "3"},
			want:      []string{"A=1", "B=2", "C=3"},
		},
		{
			name:      "override existing variable in-place",
			base:      []string{"A=1", "B=2", "C=3"},
			overrides: map[string]string{"B": "99"},
			want:      []string{"A=1", "B=99", "C=3"},
		},
		{
			name:      "unset existing variable via empty string",
			base:      []string{"A=1", "B=2", "C=3"},
			overrides: map[string]string{"B": ""},
			want:      []string{"A=1", "C=3"},
		},
		{
			name:      "unset non-existing variable",
			base:      []string{"A=1", "C=3"},
			overrides: map[string]string{"B": ""},
			want:      []string{"A=1", "C=3"},
		},
		{
			name:      "set pathenv skip",
			base:      []string{"A=1"},
			overrides: map[string]string{"PATHENV_SKIP": "1"},
			want:      []string{"A=1", "PATHENV_SKIP=1"},
		},
		{
			name:      "unset pathenv skip",
			base:      []string{"A=1", "PATHENV_SKIP=1"},
			overrides: map[string]string{"PATHENV_SKIP": ""},
			want:      []string{"A=1"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := mergeEnviron(tt.base, tt.overrides)
			if !slices.Equal(got, tt.want) {
				t.Errorf("mergeEnviron(%v, %v) = %v; want %v", tt.base, tt.overrides, got, tt.want)
			}
		})
	}
}
