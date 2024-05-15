#!/bin/bash

source ./utils.sh

version=$(get_version)
printf "Current frontend version: %s" "$version"

printf '\n\n==== Build frontend for development ====\n'
if ! build_dev; then
  printf "\n\nBuilding is failed."
else
  printf "Dev is built successfully!"
  if copy_js "$version"; then
    if ! caddy run; then
      printf "\n\nCaddy running is failed."
    fi
  fi
fi
