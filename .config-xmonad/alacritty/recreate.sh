#!/bin/bash

set -e
set -x

go run . -os linux -host transwhale -out alacritty.transwhale.yml
go run .
