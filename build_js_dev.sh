#!/bin/bash

source ./utils.sh

version=$(get_version)
printf "Current frontend version: %s" "$version"

build_js_dev_and_copy "$version"