#!/bin/bash

source ./utils.sh

version=$(get_version)
printf "Current frontend version: %s" "$version"
printf "\n\nRunning frontend in watch mode:\n\n"

caddy run -w
