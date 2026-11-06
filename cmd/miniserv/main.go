package main

import (
	"dotfiles/xmonad/cmd/miniserv/assets"
	"flag"
	"io"
	"log/slog"
	"net/http"
	"os"
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

	slog.SetDefault(slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{
		Level: slog.LevelInfo,
	})))

	var s Server

	http.Handle("/", s.Routes())

	if err := http.ListenAndServe(flags.Listen, nil); err != nil {
		slog.Error("server failed", "error", err)
		os.Exit(1)
	}
}

// Server .
type Server struct{}

func (s *Server) Routes() http.Handler {
	mux := http.NewServeMux()
	mux.Handle("/{$}", s.HandleIndex())
	mux.Handle("/style.css", s.HandleStyleCSS())
	mux.Handle("/v0/theme.css", s.HandleThemeCSS(false))
	mux.Handle("/v0/theme.json", s.HandleThemeJSON())
	mux.Handle("/v0/theme.json/{path...}", s.HandleThemeJSON())
	mux.Handle("/v0/theme/vars.css", s.HandleThemeCSS(true))
	mux.Handle("/v0/display-off", s.HandleDisplayOff())
	mux.Handle("/static/", http.FileServer(http.FS(assets.Static)))
	mux.Handle("/static", http.FileServer(http.FS(assets.Static)))
	return LoggingMiddleware(http.MaxBytesHandler(NoCacheMiddleware(mux), 1024*1024))
}

func (s *Server) HandleIndex() http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		logger := FromContext(r.Context())
		f, err := assets.Static.Open("static/index.html")
		if err != nil {
			logger.Error("opening index.html", "error", err)
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		defer f.Close()
		w.Header().Set("Content-Type", "text/html; charset=UTF-8")
		w.WriteHeader(http.StatusOK)
		if _, err := io.Copy(w, f); err != nil {
			logger.Error("copying index.html", "error", err)
			return
		}
	})
}
