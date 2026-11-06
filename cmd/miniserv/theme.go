package main

import (
	"dotfiles/xmonad/cmd/miniserv/assets"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"maps"
	"net/http"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

var customPalettePath string

func paletteFilePath() (string, error) {
	if customPalettePath != "" {
		return customPalettePath, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", err
	}
	return filepath.Join(home, ".config", "theme", "palette.json"), nil
}

// Theme contains the current theme metadata, colors, and CSS variables.
type Theme struct {
	Name      string            `json:"name,omitempty"`
	Variant   string            `json:"variant,omitempty"`
	Mode      string            `json:"mode,omitempty"`
	Colors    map[string]string `json:"colors"`
	Variables map[string]string `json:"variables,omitempty"`
	CSS       string            `json:"css,omitempty"`
	Apps      map[string]any    `json:"apps,omitempty"`
}

// readTheme loads ~/.config/theme/palette.json and returns a Theme
// with colors, CSS variables (--name), and :root CSS.
func readTheme() (*Theme, error) {
	filePath, err := paletteFilePath()
	if err != nil {
		return nil, err
	}
	data, err := os.ReadFile(filePath)
	if err != nil {
		return nil, err
	}
	var t Theme
	if err := json.Unmarshal(data, &t); err != nil {
		return nil, err
	}
	if len(t.Colors) == 0 {
		return nil, errors.New("palette.json has no colors")
	}
	t.Variables = make(map[string]string, len(t.Colors))
	for k, v := range t.Colors {
		t.Variables["--"+k] = v
	}
	t.CSS = rootCSS(t.Colors)
	return &t, nil
}

// paletteCSS returns the colors of the theme in use as CSS variables on
// :root, such as --bg_0, from the palette that dotfiles writes to
// ~/.config/theme/palette.json.
func paletteCSS() (string, error) {
	t, err := readTheme()
	if err != nil {
		return "", err
	}
	return t.CSS, nil
}

// cssVarRe matches the uses of CSS variables, such as var(--bg_0).
var cssVarRe = regexp.MustCompile(`var\(--([a-z0-9_]+)\)`)

// greyCSS returns the variables that css uses, all grey, for when the
// palette cannot be read.
func greyCSS(css []byte) string {
	colors := map[string]string{}
	for _, m := range cssVarRe.FindAllSubmatch(css, -1) {
		colors[string(m[1])] = "#808080"
	}
	return rootCSS(colors)
}

// rootCSS returns colors as CSS variables on :root.
func rootCSS(colors map[string]string) string {
	var b strings.Builder
	b.WriteString(":root {\n")
	for _, name := range slices.Sorted(maps.Keys(colors)) {
		fmt.Fprintf(&b, "  --%s: %s;\n", name, colors[name])
	}
	b.WriteString("}\n")
	return b.String()
}

func setThemeCORSHeaders(w http.ResponseWriter) {
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "GET, HEAD, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "*")
}

// HandleStyleCSS serves theme.css, after the colors of the theme in use as
// CSS variables (see paletteCSS), or grey if they cannot be read.
func (s *Server) HandleStyleCSS() http.Handler {
	return s.HandleThemeCSS(false)
}

// HandleThemeCSS serves theme CSS. If varsOnly is true or ?vars=1 is requested,
// it serves only the :root CSS variables. Otherwise, it serves the :root CSS
// variables followed by theme.css.
func (s *Server) HandleThemeCSS(varsOnly bool) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		setThemeCORSHeaders(w)
		if r.Method == http.MethodOptions {
			w.WriteHeader(http.StatusNoContent)
			return
		}
		logger := FromContext(r.Context())
		css, err := assets.Static.ReadFile("static/theme.css")
		if err != nil {
			logger.Error("reading theme.css", "error", err)
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		vars, err := paletteCSS()
		if err != nil {
			logger.Warn("no theme colors, using grey", "error", err)
			vars = greyCSS(css)
		}
		w.Header().Set("Content-Type", "text/css; charset=utf-8")
		w.WriteHeader(http.StatusOK)
		if _, err := io.WriteString(w, vars); err != nil {
			logger.Error("writing theme vars", "error", err)
			return
		}
		if varsOnly || r.URL.Query().Has("vars") || r.URL.Query().Has("variables") {
			return
		}
		if _, err := w.Write(css); err != nil {
			logger.Error("writing theme css", "error", err)
		}
	})
}

// HandleThemeJSON serves the theme in use as JSON, from the palette that
// dotfiles writes to ~/.config/theme/palette.json. If a path is provided in
// the URL (e.g. /v0/theme.json/colors/bg_0), it resolves and serves that
// specific value.
func (s *Server) HandleThemeJSON() http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		setThemeCORSHeaders(w)
		if r.Method == http.MethodOptions {
			w.WriteHeader(http.StatusNoContent)
			return
		}
		logger := FromContext(r.Context())
		t, err := readTheme()
		if err != nil {
			logger.Error("failed to read theme", "error", err)
			w.Header().Set("Content-Type", "application/json; charset=utf-8")
			w.WriteHeader(http.StatusInternalServerError)
			_ = json.NewEncoder(w).Encode(map[string]string{"error": err.Error()})
			return
		}

		subpath := strings.Trim(r.PathValue("path"), "/")
		if subpath == "" {
			w.Header().Set("Content-Type", "application/json; charset=utf-8")
			w.WriteHeader(http.StatusOK)
			enc := json.NewEncoder(w)
			enc.SetIndent("", "  ")
			if err := enc.Encode(t); err != nil {
				logger.Error("encoding theme json", "error", err)
			}
			return
		}

		val, ok := lookupThemeKey(t, subpath)
		if !ok {
			w.Header().Set("Content-Type", "application/json; charset=utf-8")
			w.WriteHeader(http.StatusNotFound)
			_ = json.NewEncoder(w).Encode(map[string]string{"error": fmt.Sprintf("key not found: %s", subpath)})
			return
		}

		if isRawRequest(r) {
			w.Header().Set("Content-Type", "text/plain; charset=utf-8")
			w.WriteHeader(http.StatusOK)
			switch v := val.(type) {
			case string:
				_, _ = io.WriteString(w, v+"\n")
			case fmt.Stringer:
				_, _ = io.WriteString(w, v.String()+"\n")
			case bool:
				_, _ = fmt.Fprintf(w, "%t\n", v)
			case int, int64, float64:
				_, _ = fmt.Fprintf(w, "%v\n", v)
			default:
				enc := json.NewEncoder(w)
				enc.SetIndent("", "  ")
				_ = enc.Encode(v)
			}
			return
		}

		w.Header().Set("Content-Type", "application/json; charset=utf-8")
		w.WriteHeader(http.StatusOK)
		enc := json.NewEncoder(w)
		enc.SetIndent("", "  ")
		if err := enc.Encode(val); err != nil {
			logger.Error("encoding theme key json", "error", err)
		}
	})
}

func isRawRequest(r *http.Request) bool {
	q := r.URL.Query()
	if q.Has("raw") {
		v := strings.ToLower(q.Get("raw"))
		return v == "" || v == "true" || v == "1" || v == "yes"
	}
	return r.Header.Get("Accept") == "text/plain"
}

func themeToMap(t *Theme) map[string]any {
	m := map[string]any{
		"name":      t.Name,
		"variant":   t.Variant,
		"mode":      t.Mode,
		"colors":    t.Colors,
		"variables": t.Variables,
		"css":       t.CSS,
	}
	if len(t.Apps) > 0 {
		m["apps"] = t.Apps
	}
	return m
}

func lookupPath(val any, path string) (any, bool) {
	parts := strings.Split(strings.Trim(path, "/"), "/")
	curr := val
	for _, part := range parts {
		if part == "" {
			continue
		}
		switch node := curr.(type) {
		case map[string]any:
			next, ok := node[part]
			if !ok {
				return nil, false
			}
			curr = next
		case map[string]string:
			next, ok := node[part]
			if !ok {
				return nil, false
			}
			curr = next
		case []any:
			idx, err := strconv.Atoi(part)
			if err != nil || idx < 0 || idx >= len(node) {
				return nil, false
			}
			curr = node[idx]
		default:
			return nil, false
		}
	}
	return curr, true
}

func lookupThemeKey(t *Theme, path string) (any, bool) {
	m := themeToMap(t)
	val, ok := lookupPath(m, path)
	if ok {
		return val, true
	}
	clean := strings.Trim(path, "/")
	if !strings.Contains(clean, "/") && clean != "" {
		if c, exists := t.Colors[clean]; exists {
			return c, true
		}
		if v, exists := t.Variables[clean]; exists {
			return v, true
		}
	}
	return nil, false
}
