package main

import (
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
)

func main() {
	batName := flag.String("bat", "BAT0", "The battery identifier (e.g., BAT0, BAT1)")
	flag.Parse()

	thresholdPath := filepath.Join("/sys", "class", "power_supply", *batName, "charge_control_end_threshold")

	if _, err := os.Stat(thresholdPath); os.IsNotExist(err) {
		fmt.Printf("Error: Battery control file not found at %s\n", thresholdPath)
		fmt.Println("Check if the battery name is correct and supported by your hardware.")
		os.Exit(1)
	}

	var targetValue string
	args := flag.Args()

	if len(args) == 0 {
		data, err := os.ReadFile(thresholdPath)
		if err != nil {
			fmt.Printf("Error reading current threshold: %v\n", err)
			os.Exit(1)
		}
		current := strings.TrimSpace(string(data))
		if current == "80" {
			targetValue = "100"
		} else {
			targetValue = "80"
		}
	} else {
		input := args[0]
		val, err := strconv.Atoi(input)
		if err != nil || val < 1 || val > 100 {
			fmt.Println("Error: Please provide a valid percentage (1-100) as the first argument.")
			os.Exit(1)
		}
		targetValue = input
	}

	// fmt.Printf("Updating %s to %s%%...\n", *batName, targetValue)
	cmd := exec.Command("sudo", "sh", "-c", fmt.Sprintf("echo %s > %s", targetValue, thresholdPath))
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	err := cmd.Run()
	if err != nil {
		fmt.Printf("Failed to apply threshold. Ensure you have sudo privileges.\n")
		os.Exit(1)
	}

	fmt.Printf("%s threshold set to %s%%.\n", *batName, targetValue)
}
