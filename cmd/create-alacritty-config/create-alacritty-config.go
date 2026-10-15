package main

import (
	"bytes"
	"embed"
	"flag"
	"fmt"
	"log"
	"os"
	"os/user"
	"path/filepath"
	"runtime"
	"strings"

	"dario.cat/mergo"
	"github.com/BurntSushi/toml"
	"github.com/davecgh/go-spew/spew"
)

// Flags .
type Flags struct {
	OS    string
	Host  string
	Theme string
	Dark  bool

	Out string
	Dir string
}

func (f *Flags) Register() {

	flag.StringVar(&f.Host, "host", Hostname(), "host name")
	flag.StringVar(&f.OS, "os", runtime.GOOS, "os name")
	flag.StringVar(&f.Theme, "theme", "selenized-light", "theme name")
	flag.BoolVar(&f.Dark, "dark", darkmode(), "use the -dark variant of a -light theme, on by default while ~/.config/darkmode exists")
	flag.StringVar(&f.Out, "out", "alacritty", "output filename (minus extension)")
	dir, err := homedir()
	if err != nil {
		log.Fatal(err)
	}
	dir = filepath.Join(dir, ".config-xmonad/alacritty/")
	flag.StringVar(&f.Dir, "dir", dir, "output directory")
}

//go:embed config
var config embed.FS

func homedir() (string, error) {
	u, err := user.Current()
	if err != nil {
		return "", err
	}
	return u.HomeDir, nil
}

func main() {

	log.SetFlags(log.LstdFlags | log.Lshortfile)
	var flags Flags
	flags.Register()

	flag.Parse()
	if base, ok := strings.CutSuffix(flags.Theme, "-light"); ok && flags.Dark {
		flags.Theme = base + "-dark"
	}

	{
		m := make(map[string]any)
		mustLoadAndMergeToml("c.common.toml", m)
		mustLoadAndMergeToml(fmt.Sprintf("c.colors.%s.toml", flags.Theme), m)
		mustLoadAndMergeIfExistsToml(fmt.Sprintf("c.os.%s.toml", flags.OS), m)
		mustLoadAndMergeIfExistsToml(fmt.Sprintf("c.host.%s.toml", flags.Host), m)

		var buf bytes.Buffer
		enc := toml.NewEncoder(&buf)

		if err := enc.Encode(&m); err != nil {
			spew.Dump(m)
			log.Fatalf("error: %v", err)
		}
		// Write to a temporary file and rename it so that a running alacritty
		// never live reloads a half written config.
		out := filepath.Join(flags.Dir, fmt.Sprintf("%s.toml", flags.Out))
		tmp := out + ".tmp"
		if err := os.WriteFile(tmp, buf.Bytes(), 0600); err != nil {
			log.Fatal(err)
		}
		if err := os.Rename(tmp, out); err != nil {
			log.Fatal(err)
		}
	}
}

// darkmode reports whether dark mode is on. The darkmode script creates
// ~/.config/darkmode while it is.
func darkmode() bool {
	dir, err := homedir()
	if err != nil {
		log.Fatal(err)
	}
	_, err = os.Stat(filepath.Join(dir, ".config/darkmode"))
	return err == nil
}

func loadAndMergeToml(filename string, m map[string]any) error {
	tomldata, err := config.ReadFile(filepath.Join("config", filename))
	if err != nil {
		return err
	}
	nm := make(map[string]any)

	err = toml.Unmarshal(tomldata, &nm)
	if err != nil {
		return err
	}
	if err := mergo.Merge(&m, nm, mergo.WithOverride); err != nil {
		return err
	}
	return nil
}

func mustLoadAndMergeToml(filename string, m map[string]any) {
	err := loadAndMergeToml(filename, m)
	if err != nil {
		log.Fatal(err)
	}
}

func mustLoadAndMergeIfExistsToml(filename string, m map[string]any) {
	err := loadAndMergeToml(filename, m)
	if err != nil {
		if os.IsNotExist(err) {
			return
		}
		log.Fatal(filename, err)
	}
}

func Hostname() string {
	hostname, err := os.Hostname()
	if err != nil {
		log.Fatal(err)
	}

	if strings.Contains(hostname, ".") {
		ss := strings.Split(hostname, ".")
		hostname = ss[0]
	}
	return hostname
}
