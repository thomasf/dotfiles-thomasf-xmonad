package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"runtime"
	"strings"

	"github.com/imdario/mergo"
	"gopkg.in/yaml.v2"
)

// Flags .
type Flags struct {
	OS    string
	Host  string
	Theme string
	Keys  string
}

func (f *Flags) Register() {

	flag.StringVar(&f.Host, "host", Hostname(), "host name")
	flag.StringVar(&f.OS, "os", runtime.GOOS, "os name")
	flag.StringVar(&f.Theme, "theme", "solarized-light", "theme name")
	flag.StringVar(&f.Keys, "keys", runtime.GOOS, "keys name")
}

func main() {

	log.SetFlags(log.LstdFlags | log.Lshortfile)
	var flags Flags
	flags.Register()

	flag.Parse()

	m := make(map[interface{}]interface{})
	mustLoadAndMerge("c.common.yml", m)
	mustLoadAndMergeIfExists(fmt.Sprintf("c.keys.%s.yml", flags.Keys), m)
	mustLoadAndMerge(fmt.Sprintf("c.colors.%s.yml", flags.Theme), m)
	mustLoadAndMergeIfExists(fmt.Sprintf("c.os.%s.yml", flags.OS), m)
	mustLoadAndMergeIfExists(fmt.Sprintf("c.host.%s.yml", flags.Host), m)

	d, err := yaml.Marshal(&m)
	if err != nil {
		log.Fatalf("error: %v", err)
	}
	ioutil.WriteFile("alacritty.yml", d, 0600)
	// ioutil.WriteFile(fmt.Sprintf("alacritty.%s.yml", Hostname()), d, 0600)

}

func mustLoadAndMerge(filename string, m map[interface{}]interface{}) {
	err := loadAndMerge(filename, m)
	if err != nil {
		log.Fatal(err)
	}
}

func mustLoadAndMergeIfExists(filename string, m map[interface{}]interface{}) {
	err := loadAndMerge(filename, m)
	if err != nil {
		if os.IsNotExist(err) {
			return
		}
		log.Fatal(err)
	}
}

func loadAndMerge(filename string, m map[interface{}]interface{}) error {
	ymldata, err := ioutil.ReadFile(filename)
	if err != nil {
		return err
	}
	nm := make(map[interface{}]interface{})
	err = yaml.Unmarshal(ymldata, &nm)
	if err != nil {
		return err
	}
	if err := mergo.Merge(&m, nm, mergo.WithOverride); err != nil {
		return err
	}
	return nil
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
