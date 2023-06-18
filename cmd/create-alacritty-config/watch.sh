#!/bin/sh

set -e

exec watchexec -e .yml,.toml -i 'alacritty.*' -- ./recreate.sh
