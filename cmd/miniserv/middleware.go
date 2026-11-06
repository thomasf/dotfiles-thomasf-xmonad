package main

import (
	"context"
	"log/slog"
	"net/http"
	"strconv"
	"time"
)

type contextKey struct{}

// WithLogger returns a new context with the logger attached.
func WithLogger(ctx context.Context, logger *slog.Logger) context.Context {
	return context.WithValue(ctx, contextKey{}, logger)
}

// FromContext extracts the logger, or falls back to slog.Default().
func FromContext(ctx context.Context) *slog.Logger {
	if l, ok := ctx.Value(contextKey{}).(*slog.Logger); ok {
		return l
	}
	return slog.Default()
}

type statusRecorder struct {
	http.ResponseWriter
	status int
	size   int
}

func (r *statusRecorder) WriteHeader(status int) {
	r.status = status
	r.ResponseWriter.WriteHeader(status)
}

func (r *statusRecorder) Write(b []byte) (int, error) {
	if r.status == 0 {
		r.status = http.StatusOK
	}
	n, err := r.ResponseWriter.Write(b)
	r.size += n
	return n, err
}

func LoggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		reqID := r.Header.Get("Request-Id")
		if reqID == "" {
			reqID = "req-" + strconv.FormatInt(time.Now().UnixNano(), 36)
		}
		w.Header().Set("Request-Id", reqID)

		logger := slog.Default().With("req_id", reqID)
		ctx := WithLogger(r.Context(), logger)

		start := time.Now()
		rec := &statusRecorder{ResponseWriter: w}

		next.ServeHTTP(rec, r.WithContext(ctx))

		status := rec.status
		if status == 0 {
			status = http.StatusOK
		}

		logger.Info("",
			"caller", "http",
			"method", r.Method,
			"url", r.URL.String(),
			"status", status,
			"size", rec.size,
			"duration", time.Since(start),
		)
	})
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
