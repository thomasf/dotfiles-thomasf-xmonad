//go:build linux

package main

import (
	"fmt"

	"github.com/jezek/xgb"
	"github.com/jezek/xgb/randr"
	"github.com/jezek/xgb/xproto"
)

// A rect is an area of the root window.
type rect struct{ x, y, w, h int }

func (r rect) contains(p point) bool {
	return p.x >= r.x && p.x < r.x+r.w && p.y >= r.y && p.y < r.y+r.h
}

type point struct{ x, y int }

// A monitor is a physical monitor, named as xrandr names its output.
type monitor struct {
	name string
	rect
}

// currentScreen returns the area of the xmonad screen that shows the
// current workspace, and the monitor it is on.
func currentScreen() (rect, monitor, error) {
	X, err := xgb.NewConn()
	if err != nil {
		return rect{}, monitor{}, err
	}
	defer X.Close()
	root := xproto.Setup(X).DefaultScreen(X).Root

	c, err := rootProperty(X, root, "_NET_CURRENT_DESKTOP")
	if err != nil {
		return rect{}, monitor{}, err
	}
	if c.Format != 32 || len(c.Value) < 4 {
		return rect{}, monitor{}, fmt.Errorf("no current desktop")
	}
	cur := int(xgb.Get32(c.Value))
	vp, err := rootProperty(X, root, "_NET_DESKTOP_VIEWPORT")
	if err != nil {
		return rect{}, monitor{}, err
	}
	var origins []point
	for i := 0; i+8 <= len(vp.Value); i += 8 {
		origins = append(origins, point{int(xgb.Get32(vp.Value[i:])), int(xgb.Get32(vp.Value[i+4:]))})
	}
	if cur >= len(origins) {
		return rect{}, monitor{}, fmt.Errorf("no viewport for desktop %d", cur)
	}
	mons, err := monitors(X, root)
	if err != nil {
		return rect{}, monitor{}, err
	}
	return screenAt(origins[cur], origins, mons)
}

// screenAt returns the area of the xmonad screen whose top left corner
// is o, and its monitor. xmonad screens can be smaller than monitors,
// as when layoutScreens splits one, and xmonad publishes only their
// corners, in _NET_DESKTOP_VIEWPORT; origins holds those of all
// workspaces, among them every visible screen's. So a screen is taken to
// end where the next screen on its monitor starts, to the right or
// below, or else at the edge of the monitor.
func screenAt(o point, origins []point, mons []monitor) (rect, monitor, error) {
	const near = 16 // pixels between corners on one row or column
	for _, m := range mons {
		if !m.contains(o) {
			continue
		}
		right, bottom := m.x+m.w, m.y+m.h
		for _, p := range origins {
			if !m.contains(p) {
				continue
			}
			if p.x > o.x && abs(p.y-o.y) < near {
				right = min(right, p.x)
			}
			if p.y > o.y && abs(p.x-o.x) < near {
				bottom = min(bottom, p.y)
			}
		}
		return rect{o.x, o.y, right - o.x, bottom - o.y}, m, nil
	}
	return rect{}, monitor{}, fmt.Errorf("no monitor at %d,%d", o.x, o.y)
}

func abs(n int) int {
	return max(n, -n)
}

// monitors returns the active monitors.
func monitors(X *xgb.Conn, root xproto.Window) ([]monitor, error) {
	if err := randr.Init(X); err != nil {
		return nil, err
	}
	res, err := randr.GetScreenResourcesCurrent(X, root).Reply()
	if err != nil {
		return nil, err
	}
	var mons []monitor
	for _, crtc := range res.Crtcs {
		info, err := randr.GetCrtcInfo(X, crtc, res.ConfigTimestamp).Reply()
		if err != nil || info.Width == 0 || len(info.Outputs) == 0 {
			continue
		}
		out, err := randr.GetOutputInfo(X, info.Outputs[0], res.ConfigTimestamp).Reply()
		if err != nil {
			continue
		}
		mons = append(mons, monitor{string(out.Name), rect{int(info.X), int(info.Y), int(info.Width), int(info.Height)}})
	}
	return mons, nil
}
