#!/bin/bash

source ./scripts/utils.sh

version=$(get_version)
printf "Current frontend version: %s" "$version"

printf '\n\n==== Build frontend for development ====\n'
if ! build_dev; then
  printf "\n\nBuilding was failed.\n"
  exit 1
else
  printf "Dev v$version was built successfully!"
  if ! copy_js_8107 "$version"; then
    printf "\n\nCoping was failed.\n"
    exit 1
  else 
    # optimize_all;
    printf "\n\nLaunch caddy server\n\n"
    if ! caddy run; then
      printf "\n\nCaddy running was failed."
      exit 1
    fi
  fi
fi
