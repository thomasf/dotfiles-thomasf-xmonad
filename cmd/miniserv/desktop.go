package main

import (
	"context"
	"errors"
	"net/http"
	"os"
	"os/exec"
	"reflect"
	"runtime"
)

func (s *Server) HandleDisplayOff() http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		logger := FromContext(r.Context())
		if err := displayOff(r.Context()); err != nil {
			logger.Error("displayOff", "error", err)
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
