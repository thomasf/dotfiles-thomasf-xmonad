package main

import (
	"bytes"
	"embed"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/user"
	"path/filepath"
	"runtime"
	"strings"

	"dario.cat/mergo"
	"github.com/BurntSushi/toml"
	"github.com/davecgh/go-spew/spew"
	"gopkg.in/yaml.v3"
)

// Flags .
type Flags struct {
	OS    string
	Host  string
	Theme string

	Out string
	Dir string
}

func (f *Flags) Register() {

	flag.StringVar(&f.Host, "host", Hostname(), "host name")
	flag.StringVar(&f.OS, "os", runtime.GOOS, "os name")
	flag.StringVar(&f.Theme, "theme", "solarized-light", "theme name")
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

	{
		m := make(map[any]any)
		mustLoadAndMergeYaml("c.common.yml", m)
		mustLoadAndMergeYaml(fmt.Sprintf("c.colors.%s.yml", flags.Theme), m)
		mustLoadAndMergeIfExistsYaml(fmt.Sprintf("c.os.%s.yml", flags.OS), m)
		mustLoadAndMergeIfExistsYaml(fmt.Sprintf("c.host.%s.yml", flags.Host), m)

		var buf bytes.Buffer
		enc := yaml.NewEncoder(&buf)
		enc.SetIndent(2)

		if err := enc.Encode(&m); err != nil {
			log.Fatalf("error: %v", err)
		}
		ioutil.WriteFile(filepath.Join(flags.Dir, fmt.Sprintf("%s.yml", flags.Out)), buf.Bytes(), 0600)
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
		ioutil.WriteFile(filepath.Join(flags.Dir, fmt.Sprintf("%s.toml", flags.Out)), buf.Bytes(), 0600)
	}
}

func mustLoadAndMergeYaml(filename string, m map[any]any) {
	err := loadAndMergeYaml(filename, m)
	if err != nil {
		log.Fatal(err)
	}
}

func mustLoadAndMergeIfExistsYaml(filename string, m map[any]any) {
	err := loadAndMergeYaml(filename, m)
	if err != nil {
		if os.IsNotExist(err) {
			return
		}
		log.Fatal(filename, err)
	}
}

func loadAndMergeYaml(filename string, m map[any]any) error {
	ymldata, err := config.ReadFile(filepath.Join("config", filename))
	if err != nil {
		return err
	}
	nm := make(map[any]any)
	err = yaml.Unmarshal(ymldata, &nm)
	if err != nil {
		return err
	}
	if err := mergo.Merge(&m, nm, mergo.WithOverride); err != nil {
		return err
	}
	return nil
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
