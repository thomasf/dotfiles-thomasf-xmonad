package main

import (
	"bytes"
	"context"
	"flag"
	"fmt"
	"image"
	"image/draw"
	"image/png"
	_ "image/png" // register the PNG format with the image package
	"os"
	"os/exec"
	"path/filepath"
	"time"

	"github.com/mandykoh/autocrop"
)

// Flags .
type Flags struct {
	Out    string
	Noclip bool
}

func (f *Flags) Register(fs *flag.FlagSet) {
	fs.StringVar(&f.Out, "out", "", "output filename, autochosen if empty")
	fs.BoolVar(&f.Noclip, "noclip", false, "don not save to clipboard")
}

func main() {

	var flags Flags

	flags.Register(flag.CommandLine)
	flag.Parse()

	ctx := context.Background()

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
			fmt.Sprintf("%s selection.png", time.Now().Format("2006-01-02  15 04 05")))

	}
	if err := sshot(ctx, filename, !flags.Noclip); err != nil {
		fmt.Fprintln(os.Stderr, "ERROR", err)
		os.Exit(1)
	}
}

func sshot(ctx context.Context, filename string, clip bool) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	maimCmd := exec.CommandContext(ctx, "maim", "-f", "png", "-s", "-u")

	maimCmd.Stderr = os.Stderr
	var buf bytes.Buffer
	maimCmd.Stdout = &buf

	if err := maimCmd.Run(); err != nil {
		return err
	}

	img, _, err := image.Decode(&buf)
	if err != nil {
		return err
	}

	resultImg := smartCrop(img)

	buf.Reset()
	if err := png.Encode(&buf, resultImg); err != nil {
		return err
	}
	pngData := buf.Bytes()

	if err := os.WriteFile(filename, pngData, 0o600); err != nil {
		return err
	}

	if clip {
		cmdXclip := exec.CommandContext(ctx, "xclip", "-selection", "c", "-t", "image/png")
		cmdXclip.Stdin = bytes.NewReader(pngData)
		cmdXclip.Stderr = os.Stderr
		if err := cmdXclip.Run(); err != nil {
			return err
		}
	}

	return nil

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
