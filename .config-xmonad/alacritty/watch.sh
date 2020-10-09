#!/bin/sh

set -e

exec watchexec -e .yml -i 'alacritty.*' -- ./recreate.sh
