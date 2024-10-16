// recordwindow records an X11 window using ffmpeg.
//
// NOTE: This program is mostly written by chatgpt.
package main

import (
	"bufio"
	"context"
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"

	"github.com/rs/xid"
)

func main() {
	output := flag.String("out", fmt.Sprintf("%s__%s.mp4", time.Now().Format("2006-01-02__15-04"), xid.New().String()), "Output file name")
	includeAudio := flag.Bool("audio", false, "record audio")
	sinkName := flag.String("sink", "alsa_output.usb-Roland_UA-22-00.analog-stereo.monitor", "PulseAudio sink name")
	listSourcesFlag := flag.Bool("sources", false, "List available PulseAudio sources")

	flag.Parse()

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	if *listSourcesFlag {
		if err := listSources(ctx); err != nil {
			log.Fatalf("Error listing sources: %v", err)
		}
		return
	}

	x, y, width, height, err := getWindowGeometry(ctx)
	if err != nil {
		log.Fatalf("Failed to get window geometry: %v", err)
	}

	var args = []string{
		"-f", "x11grab",
		"-video_size", fmt.Sprintf("%dx%d", width, height),
		"-framerate", "25",
		"-i", fmt.Sprintf(":0.0+%d,%d", x, y),
	}

	if *includeAudio {
		monitorIndex, err := getMonitorForSink(*sinkName)
		if err != nil {
			log.Fatalf("Failed to find monitor for sink %s: %v", *sinkName, err)
		}
		args = append(args,
			"-f", "pulse",
			"-ac", "2",
			"-i", monitorIndex,
			"-probesize", "200M",
		)
	}
	args = append(args, *output)

	if err := runFfmpeg(ctx, args); err != nil {
		log.Fatalf("ffmpeg finished with error: %v", err)
	}

}

func runFfmpeg(ctx context.Context, args []string) error {
	ctx, cancel := context.WithTimeout(ctx, 2*time.Hour) // set some limit
	defer cancel()
	cmd := exec.CommandContext(ctx, "ffmpeg", args...)
	cmd.Stderr = os.Stderr
	cmd.Stdout = os.Stdout
	cmd.Stdin = os.Stdin

	return cmd.Run()

}

func getWindowGeometry(ctx context.Context) (x, y, width, height int, err error) {
	ctx, cancel := context.WithTimeout(ctx, time.Minute)
	defer cancel()
	xwininfoCmd := exec.CommandContext(ctx, "xwininfo", "-frame")
	xwininfoOutput, err := xwininfoCmd.Output()
	if err != nil {
		return 0, 0, 0, 0, fmt.Errorf("failed to run xwininfo: %w", err)
	}

	scanner := bufio.NewScanner(strings.NewReader(string(xwininfoOutput)))
	for scanner.Scan() {
		line := scanner.Text()
		if strings.Contains(line, "Absolute upper-left X:") {
			x, err = strconv.Atoi(strings.Fields(line)[3])
			if err != nil {
				return 0, 0, 0, 0, fmt.Errorf("failed to parse X coordinate: %w", err)
			}
		} else if strings.Contains(line, "Absolute upper-left Y:") {
			y, err = strconv.Atoi(strings.Fields(line)[3])
			if err != nil {
				return 0, 0, 0, 0, fmt.Errorf("failed to parse Y coordinate: %w", err)
			}
		} else if strings.Contains(line, "Width:") {
			width, err = strconv.Atoi(strings.Fields(line)[1])
			if err != nil {
				return 0, 0, 0, 0, fmt.Errorf("failed to parse width: %w", err)
			}
		} else if strings.Contains(line, "Height:") {
			height, err = strconv.Atoi(strings.Fields(line)[1])
			if err != nil {
				return 0, 0, 0, 0, fmt.Errorf("failed to parse height: %w", err)
			}
		}
	}

	if err := scanner.Err(); err != nil {
		return 0, 0, 0, 0, fmt.Errorf("error reading xwininfo output: %w", err)
	}

	return x, y, width, height, nil
}

func listSources(ctx context.Context) error {
	ctx, cancel := context.WithTimeout(ctx, 10*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "pactl", "list", "short", "sources")
	output, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("failed to list PulseAudio sources: %w", err)
	}

	fmt.Println("Available PulseAudio Sources:")
	fmt.Println(strings.TrimSpace(string(output)))
	return nil
}

func getMonitorForSink(sink string) (string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "pactl", "list", "short", "sources")
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to list PulseAudio sources: %w", err)
	}
	scanner := bufio.NewScanner(strings.NewReader(string(output)))
	var monitorIndex string
	for scanner.Scan() {
		line := scanner.Text()
		fields := strings.Fields(line)
		if len(fields) >= 3 && fields[1] == sink {
			monitorIndex = fields[0]
			break
		}
	}
	if err := scanner.Err(); err != nil {
		return "", fmt.Errorf("error reading PulseAudio sources: %w", err)
	}
	if monitorIndex == "" {
		return "", fmt.Errorf("no monitor source found for sink %s", sink)
	}
	return monitorIndex, nil
}
