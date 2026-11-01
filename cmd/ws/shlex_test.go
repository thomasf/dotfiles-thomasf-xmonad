//go:build linux

package main

import (
	"slices"
	"testing"
)

func TestSplitCommand(t *testing.T) {
	tests := []struct {
		input   string
		want    []string
		wantErr bool
	}{
		{
			input: "htop",
			want:  []string{"htop"},
		},
		{
			input: "  ping   8.8.8.8  ",
			want:  []string{"ping", "8.8.8.8"},
		},
		{
			input: `echo "hello world"`,
			want:  []string{"echo", "hello world"},
		},
		{
			input: `echo 'foo bar' baz`,
			want:  []string{"echo", "foo bar", "baz"},
		},
		{
			input: `git commit -m "commit message"`,
			want:  []string{"git", "commit", "-m", "commit message"},
		},
		{
			input: `echo ""`,
			want:  []string{"echo", ""},
		},
		{
			input: `git commit -m ''`,
			want:  []string{"git", "commit", "-m", ""},
		},
		{
			input: `""`,
			want:  []string{""},
		},
		{
			input:   `echo "unclosed`,
			wantErr: true,
		},
		{
			input: "",
			want:  nil,
		},
		{
			input: "   ",
			want:  nil,
		},
	}

	for _, tc := range tests {
		got, err := splitCommand(tc.input)
		if (err != nil) != tc.wantErr {
			t.Errorf("splitCommand(%q) err = %v, wantErr = %v", tc.input, err, tc.wantErr)
			continue
		}
		if !slices.Equal(got, tc.want) {
			t.Errorf("splitCommand(%q) = %v, want %v", tc.input, got, tc.want)
		}
	}
}
