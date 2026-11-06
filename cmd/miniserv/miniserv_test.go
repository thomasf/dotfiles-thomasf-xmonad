package main

import (
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func createTestPalette(t *testing.T) string {
	t.Helper()
	tmpDir := t.TempDir()
	pPath := filepath.Join(tmpDir, "palette.json")
	content := `{
  "name": "test-theme",
  "variant": "dark",
  "mode": "dark",
  "colors": {
    "bg_0": "#112233",
    "fg_0": "#aabbcc",
    "red": "#ff0000"
  },
  "apps": {
    "font": "mono"
  }
}`
	if err := os.WriteFile(pPath, []byte(content), 0644); err != nil {
		t.Fatalf("failed to write test palette: %v", err)
	}
	return pPath
}

func TestThemeEndpoints(t *testing.T) {
	palettePath := createTestPalette(t)
	customPalettePath = palettePath
	defer func() { customPalettePath = "" }()

	var s Server
	router := s.Routes()

	tests := []struct {
		name         string
		url          string
		method       string
		accept       string
		expectedCode int
		contentType  string
		checkBody    func(t *testing.T, body string)
	}{
		{
			name:         "v0/theme.json returns JSON",
			url:          "/v0/theme.json",
			expectedCode: http.StatusOK,
			contentType:  "application/json",
			checkBody: func(t *testing.T, body string) {
				var theme Theme
				if err := json.Unmarshal([]byte(body), &theme); err != nil {
					t.Fatalf("unmarshal error: %v", err)
				}
				if theme.Name != "test-theme" {
					t.Errorf("expected name test-theme, got %s", theme.Name)
				}
				if theme.Colors["bg_0"] != "#112233" {
					t.Errorf("expected bg_0 #112233, got %s", theme.Colors["bg_0"])
				}
				if theme.Variables["--bg_0"] != "#112233" {
					t.Errorf("expected --bg_0 #112233, got %s", theme.Variables["--bg_0"])
				}
				if !strings.Contains(theme.CSS, "--bg_0: #112233;") {
					t.Errorf("expected CSS to contain --bg_0: #112233, got %s", theme.CSS)
				}
			},
		},
		{
			name:         "v0/theme.json/colors/bg_0 returns JSON string",
			url:          "/v0/theme.json/colors/bg_0",
			expectedCode: http.StatusOK,
			contentType:  "application/json",
			checkBody: func(t *testing.T, body string) {
				if strings.TrimSpace(body) != `"#112233"` {
					t.Errorf("expected \"#112233\", got %s", body)
				}
			},
		},
		{
			name:         "v0/theme.json/colors/bg_0?raw returns plain text without quotes",
			url:          "/v0/theme.json/colors/bg_0?raw",
			expectedCode: http.StatusOK,
			contentType:  "text/plain",
			checkBody: func(t *testing.T, body string) {
				if strings.TrimSpace(body) != `#112233` {
					t.Errorf("expected #112233, got %s", body)
				}
			},
		},
		{
			name:         "v0/theme.json/colors/bg_0?raw=true returns plain text without quotes",
			url:          "/v0/theme.json/colors/bg_0?raw=true",
			expectedCode: http.StatusOK,
			contentType:  "text/plain",
			checkBody: func(t *testing.T, body string) {
				if strings.TrimSpace(body) != `#112233` {
					t.Errorf("expected #112233, got %s", body)
				}
			},
		},
		{
			name:         "v0/theme.json/colors/bg_0?raw=false returns quoted JSON",
			url:          "/v0/theme.json/colors/bg_0?raw=false",
			expectedCode: http.StatusOK,
			contentType:  "application/json",
			checkBody: func(t *testing.T, body string) {
				if strings.TrimSpace(body) != `"#112233"` {
					t.Errorf("expected \"#112233\", got %s", body)
				}
			},
		},
		{
			name:         "v0/theme.json/colors returns colors map JSON",
			url:          "/v0/theme.json/colors",
			expectedCode: http.StatusOK,
			contentType:  "application/json",
			checkBody: func(t *testing.T, body string) {
				var colors map[string]string
				if err := json.Unmarshal([]byte(body), &colors); err != nil {
					t.Fatalf("unmarshal colors: %v", err)
				}
				if colors["red"] != "#ff0000" {
					t.Errorf("expected red #ff0000, got %s", colors["red"])
				}
			},
		},
		{
			name:         "v0/theme.json/apps/font returns font value",
			url:          "/v0/theme.json/apps/font",
			expectedCode: http.StatusOK,
			contentType:  "application/json",
			checkBody: func(t *testing.T, body string) {
				if strings.TrimSpace(body) != `"mono"` {
					t.Errorf("expected \"mono\", got %s", body)
				}
			},
		},
		{
			name:         "v0/theme.json/bg_0 shortcut to color",
			url:          "/v0/theme.json/bg_0",
			expectedCode: http.StatusOK,
			contentType:  "application/json",
			checkBody: func(t *testing.T, body string) {
				if strings.TrimSpace(body) != `"#112233"` {
					t.Errorf("expected \"#112233\", got %s", body)
				}
			},
		},
		{
			name:         "v0/theme.json/invalid/path returns 404",
			url:          "/v0/theme.json/invalid/path",
			expectedCode: http.StatusNotFound,
			contentType:  "application/json",
			checkBody: func(t *testing.T, body string) {
				if !strings.Contains(body, "key not found") {
					t.Errorf("expected error key not found, got %s", body)
				}
			},
		},
		{
			name:         "v0/theme.css returns full stylesheet",
			url:          "/v0/theme.css",
			expectedCode: http.StatusOK,
			contentType:  "text/css",
			checkBody: func(t *testing.T, body string) {
				if !strings.Contains(body, "--bg_0: #112233;") {
					t.Errorf("expected variables in CSS, got %s", body)
				}
				if !strings.Contains(body, "article") {
					t.Errorf("expected full stylesheet, got %s", body)
				}
			},
		},
		{
			name:         "v0/theme.css?vars=1 returns only variables",
			url:          "/v0/theme.css?vars=1",
			expectedCode: http.StatusOK,
			contentType:  "text/css",
			checkBody: func(t *testing.T, body string) {
				if !strings.Contains(body, "--bg_0: #112233;") {
					t.Errorf("expected variables in CSS, got %s", body)
				}
				if strings.Contains(body, "article") {
					t.Errorf("expected vars only, got %s", body)
				}
			},
		},
		{
			name:         "v0/theme/vars.css returns only variables",
			url:          "/v0/theme/vars.css",
			expectedCode: http.StatusOK,
			contentType:  "text/css",
			checkBody: func(t *testing.T, body string) {
				if !strings.Contains(body, "--bg_0: #112233;") {
					t.Errorf("expected variables in CSS, got %s", body)
				}
				if strings.Contains(body, "article") {
					t.Errorf("expected vars only, got %s", body)
				}
			},
		},
		{
			name:         "style.css returns full stylesheet",
			url:          "/style.css",
			expectedCode: http.StatusOK,
			contentType:  "text/css",
			checkBody: func(t *testing.T, body string) {
				if !strings.Contains(body, "--bg_0: #112233;") {
					t.Errorf("expected variables in CSS, got %s", body)
				}
				if !strings.Contains(body, "article") {
					t.Errorf("expected full stylesheet, got %s", body)
				}
			},
		},
		{
			name:         "CORS preflight OPTIONS request",
			url:          "/v0/theme.json",
			method:       http.MethodOptions,
			expectedCode: http.StatusNoContent,
			checkBody: func(t *testing.T, body string) {
				// StatusNoContent has empty body
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			method := tt.method
			if method == "" {
				method = http.MethodGet
			}
			req := httptest.NewRequest(method, tt.url, nil)
			if tt.accept != "" {
				req.Header.Set("Accept", tt.accept)
			}
			rec := httptest.NewRecorder()
			router.ServeHTTP(rec, req)

			if rec.Code != tt.expectedCode {
				t.Fatalf("expected status %d, got %d. Body: %s", tt.expectedCode, rec.Code, rec.Body.String())
			}
			if tt.contentType != "" {
				ct := rec.Header().Get("Content-Type")
				if !strings.HasPrefix(ct, tt.contentType) {
					t.Errorf("expected content-type prefix %s, got %s", tt.contentType, ct)
				}
			}
			cors := rec.Header().Get("Access-Control-Allow-Origin")
			if cors != "*" {
				t.Errorf("expected CORS header *, got %s", cors)
			}
			if tt.checkBody != nil {
				tt.checkBody(t, rec.Body.String())
			}
		})
	}
}

func TestThemeEndpointsMissingPalette(t *testing.T) {
	customPalettePath = "/path/to/nonexistent/palette.json"
	defer func() { customPalettePath = "" }()

	var s Server
	router := s.Routes()

	// JSON request should return 500
	req := httptest.NewRequest(http.MethodGet, "/v0/theme.json", nil)
	rec := httptest.NewRecorder()
	router.ServeHTTP(rec, req)
	if rec.Code != http.StatusInternalServerError {
		t.Errorf("expected 500 for missing palette json, got %d", rec.Code)
	}

	// CSS request should fall back to grey and return 200
	req = httptest.NewRequest(http.MethodGet, "/v0/theme.css", nil)
	rec = httptest.NewRecorder()
	router.ServeHTTP(rec, req)
	if rec.Code != http.StatusOK {
		t.Errorf("expected 200 for missing palette css fallback, got %d", rec.Code)
	}
	if !strings.Contains(rec.Body.String(), "#808080") {
		t.Errorf("expected grey fallback in CSS body, got %s", rec.Body.String())
	}
}
