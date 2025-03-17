#!/bin/bash

set -e
set -x

go run . -os linux -host transwhale -out alacritty.transwhale
go run . -os linux -host flam -out alacritty.flam
go run .
