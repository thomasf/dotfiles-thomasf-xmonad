package main

import (
	"bytes"
	"context"
	"encoding/xml"
	"flag"
	"fmt"
	"image"
	"image/draw"
	"image/png"
	_ "image/png" // register the PNG format with the image package
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"time"

	"github.com/mandykoh/autocrop"
)

// Flags .
type Flags struct {
	Out    string
	NoClip bool
	NoCrop bool
}

func (f *Flags) Register(fs *flag.FlagSet) {
	fs.StringVar(&f.Out, "out", "", "output filename, autochosen if empty")
	fs.BoolVar(&f.NoClip, "noclip", false, "don not save to clipboard")
	fs.BoolVar(&f.NoCrop, "nocrop", false, "don not auto crop image")
}

func main() {

	var fs = flag.CommandLine
	var flags Flags

	fs.Usage = func() {
		fmt.Fprintf(fs.Output(), "Usage of %s -args subcommand:\n", os.Args[0])
		flag.PrintDefaults()
		fmt.Fprint(fs.Output(), `
Subcommands:
   select : select region with mouse
   full: full srreen
   focused:  currently focused window

`)
	}

	flags.Register(flag.CommandLine)
	flag.Parse()

	args := flag.CommandLine.Args()
	subcommand := "select"
	if len(args) > 0 {
		subcommand = args[0]
	}

	filename := flags.Out
	if filename == "" {
		homedir, err := os.UserHomeDir()
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}

		filename = filepath.Join(
			homedir,
			"Pictures",
			"scrot",
			fmt.Sprintf("%s %s.png", time.Now().Format("2006-01-02 15_04_05"), subcommand))

	}

	ctx := context.Background()

	var data []byte
	var err error
	switch subcommand {
	case "select":
		data, err = selectScreenshot(ctx, !flags.NoCrop)
	case "full":
		data, err = fullScreenshot(ctx)
	case "active", "focused":
		data, err = activeWindowScreenshot(ctx, !flags.NoCrop)
	default:
		fmt.Fprintln(os.Stderr, subcommand, "is an invalid subcommand")

	}
	if err != nil {
		fmt.Fprintln(os.Stderr, "ERROR", err)
		os.Exit(1)
	}

	if filename != "" {
		if err := os.WriteFile(filename, data, 0o600); err != nil {
			fmt.Fprintln(os.Stderr, "ERROR", err)
			os.Exit(1)

		}
	}

	if !flags.NoClip && data != nil {
		if err := xclip(ctx, data); err != nil {
			fmt.Fprintln(os.Stderr, "ERROR", err)
			os.Exit(1)
		}
	}

	notify(ctx, "saved "+filepath.Base(filename))
}

func selectScreenshot(ctx context.Context, crop bool) ([]byte, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	img, err := maim(ctx, "-s")
	if err != nil {
		return nil, err
	}
	if crop {
		img = smartCrop(img)
	}
	var buf bytes.Buffer
	if err := png.Encode(&buf, img); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func fullScreenshot(ctx context.Context) ([]byte, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	img, err := maim(ctx, "-B")
	if err != nil {
		return nil, err
	}

	var buf bytes.Buffer
	if err := png.Encode(&buf, img); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func activeWindowScreenshot(ctx context.Context, crop bool) ([]byte, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	window, err := activeWindow(ctx)
	if err != nil {
		return nil, err
	}
	img, err := maim(ctx,
		"-i", window,
	)
	if err != nil {
		return nil, err
	}

	nrgbaImg := image.NewNRGBA(image.Rect(0, 0, img.Bounds().Dx(), img.Bounds().Dy()))
	draw.Draw(nrgbaImg, nrgbaImg.Bounds(), img, img.Bounds().Min, draw.Src)
	if crop {
		nrgbaImg = autocrop.ToThreshold(nrgbaImg, 0.01)
		nrgbaImg = smartCrop(nrgbaImg)
	}

	var buf bytes.Buffer
	if err := png.Encode(&buf, nrgbaImg); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func maim(ctx context.Context, args ...string) (image.Image, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	allArgs := append(args, "-f", "png", "-u")
	cmd := exec.CommandContext(ctx, "maim", allArgs...)
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	img, _, err := image.Decode(&buf)
	if err != nil {
		return nil, err
	}
	return img, nil
}

func notify(ctx context.Context, message string) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	var safeMessage = "error escaping message string"
	var buf bytes.Buffer

	if err := xml.EscapeText(&buf, []byte(message)); err != nil {
		log.Println(err)
	} else {
		safeMessage = buf.String()
	}
	cmd := exec.CommandContext(ctx,
		"notify-send",
		"-u", "normal",
		"-a", "shorter",
		"sshot",
		fmt.Sprintf("<span font='8'>%s</span>", safeMessage),
	)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	return nil
}

func xclip(ctx context.Context, data []byte) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	cmd := exec.CommandContext(ctx,
		"xclip",
		"-selection", "c",
		"-t", "image/png",
	)
	cmd.Stdin = bytes.NewReader(data)
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	return nil
}

func activeWindow(ctx context.Context) (string, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	cmd := exec.CommandContext(ctx, "xdotool", "getactivewindow")
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return "", err
	}
	return buf.String(), nil
}

func smartCrop(img image.Image) *image.NRGBA {
	nrgbaImg := image.NewNRGBA(image.Rect(0, 0, img.Bounds().Dx(), img.Bounds().Dy()))
	draw.Draw(nrgbaImg, nrgbaImg.Bounds(), img, img.Bounds().Min, draw.Src)
	crop := autocrop.BoundsForThreshold(nrgbaImg, 0.02)

	const padding = 16
	orig := nrgbaImg.Bounds()
	bounds := image.Rect(
		max(orig.Min.X, crop.Min.X-padding),
		max(orig.Min.Y, crop.Min.Y-padding),
		min(orig.Max.X, crop.Max.X+padding),
		min(orig.Max.Y, crop.Max.Y+padding),
	)

	resultImg := image.NewNRGBA(image.Rect(0, 0, bounds.Dx(), bounds.Dy()))
	draw.Draw(resultImg, resultImg.Bounds(), nrgbaImg, bounds.Min, draw.Src)
	return resultImg
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
