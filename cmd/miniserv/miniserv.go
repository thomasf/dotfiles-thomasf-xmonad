package main

import (
	"context"
	"dotfiles/xmonad/cmd/miniserv/assets"
	"encoding/json"
	"errors"
	"flag"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"runtime"
	"time"

	"github.com/gorilla/mux"
	"github.com/justinas/alice"
	"github.com/rs/zerolog/hlog"
	"github.com/rs/zerolog/log"
)

// Flags .
type Flags struct {
	Listen string
}

func (f *Flags) Register(fs *flag.FlagSet) {
	fs.StringVar(&f.Listen, "listen", ":7345", "listening address")
}

func main() {
	var flags Flags

	flags.Register(flag.CommandLine)

	flag.Parse()

	var s Server

	http.Handle("/", s.Routes())

	if err := http.ListenAndServe(flags.Listen, nil); err != nil {
		log.Fatal().Err(err).Msg("")
	}
}

// Server .
type Server struct{}

func (s *Server) Routes() *mux.Router {
	c := alice.New()
	c = c.Append(
		hlog.NewHandler(log.Logger),
	)
	c = c.Append(
		hlog.RequestIDHandler("req_id", "Request-Id"),
		hlog.AccessHandler(func(r *http.Request, status, size int, duration time.Duration) {
			l := log.Logger
			l.Info().
				Str("caller", "http").
				Str("method", r.Method).
				Stringer("url", r.URL).
				Int("status", status).
				Int("size", size).
				Dur("duration", duration).
				Msg("")
		}),
		MaxBytesReaderMiddleware(1024*1024),
		NoCacheMiddleware,
	)
	r := mux.NewRouter()
	r.Handle("/", c.Then(s.HandleIndex()))
	r.Handle("/style.css", c.Then(s.HandleStyleCSS()))
	r.Handle("/static", c.Then(http.FileServer(http.FS(assets.Static))))
	r.Handle("/v0/display-off", c.Then(s.HandleDisplayOff()))
	return r
}

func (s *Server) HandleIndex() http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		logger := log.Ctx(r.Context())
		f, err := assets.Static.Open("static/index.html")
		if err != nil {
			logger.Error().Err(err).Msg("")
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		defer f.Close()
		w.Header().Set("Content-Type", "text/html; charset=UTF-8")
		w.WriteHeader(http.StatusOK)
		if _, err := io.Copy(w, f); err != nil {
			logger.Error().Err(err).Msg("")
			return
		}
	})
}

func (s *Server) HandleStyleCSS() http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		logger := log.Ctx(r.Context())
		filename := "static/solarized-light.css"
		homeDir, err := os.UserHomeDir()
		if err != nil {
			log.Error().Err(err).Msg("")
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		if fileExists(filepath.Join(homeDir, ".config", "darkmode")) {
			filename = "static/solarized-dark.css"
		}
		f, err := assets.Static.Open(filename)
		if err != nil {
			logger.Error().Err(err).Msg("")
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		defer f.Close()
		w.Header().Set("Content-Type", "text/css")
		w.WriteHeader(http.StatusOK)
		if _, err := io.Copy(w, f); err != nil {
			logger.Error().Err(err).Msg("")
			return

		}
	})
}

func (s *Server) HandleDisplayOff() http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		ctx := r.Context()
		logger := log.Ctx(ctx)
		if err := displayOff(ctx); err != nil {
			logger.Error().Err(err).Msg("")
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		w.WriteHeader(http.StatusOK)
	})
}

// errorInfo .
type errorInfo struct {
	Error string `json:"err"`
	Type  string `json:"type"`
}

func inspectError(err error) []errorInfo {
	var info []errorInfo
	e := err
	for e != nil {
		t := reflect.TypeOf(e)
		info = append(info, errorInfo{
			Error: e.Error(),
			Type:  t.String(),
		})
		e = errors.Unwrap(e)
	}
	return info
}

type AppHandler func(http.ResponseWriter, *http.Request) error

func (fn AppHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if err := fn(w, r); err != nil {
		logger := log.Ctx(r.Context())
		errInfo := inspectError(err)
		data, err := json.Marshal(&errInfo)
		w.WriteHeader(500)
		if err != nil {
			logger.Err(err).Msg("marshalling inspect error")
			w.Write([]byte("[]"))
		} else {
			logger.Err(err).RawJSON("errors", data).Msg("error info")
			w.Write(data)
		}
	}
}

// NoCacheMiddleware just sets various http headers to ensure that browsers do
// not try to cache dynamic pages.
func NoCacheMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cache-Control", "no-cache, no-store, must-revalidate")
		w.Header().Set("Pragma", "no-cache")
		w.Header().Set("Expires", "0")
		next.ServeHTTP(w, r)
	})
}

// LongCacheMiddleware sets cache-control max-age to a large enough value.
func LongCacheMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Cache-Control", "max-age=604800")
		next.ServeHTTP(w, r)
	})
}

// maxBytesReaderMiddleware .
type maxBytesReaderMiddleware struct {
	h http.Handler
	N int64
}

func (b maxBytesReaderMiddleware) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	r.Body = http.MaxBytesReader(w, r.Body, b.N)
	b.h.ServeHTTP(w, r)
}

func MaxBytesReaderMiddleware(maxSize int64) func(h http.Handler) http.Handler {
	if maxSize <= 0 {
		log.Fatal().Msgf("maxSize cannot be equal or less than 0: %v", maxSize)
	}
	fn := func(h http.Handler) http.Handler {
		return maxBytesReaderMiddleware{h: h, N: maxSize}
	}
	return fn
}

func fileExists(filename string) bool {
	info, err := os.Stat(filename)
	if os.IsNotExist(err) {
		return false
	}
	return !info.IsDir()
}

func displayOff(ctx context.Context) error {
	var cmd *exec.Cmd
	switch runtime.GOOS {
	case "linux":
		cmd = exec.CommandContext(ctx, "xset", "-display", ":0.0", "dpms", "force", "off")
	case "darwin":
		cmd = exec.CommandContext(ctx, "pmset", "displaysleepnow")
	default:
		return ErrUnsupported
	}
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

var ErrUnsupported = errors.New("unsupported operation")
