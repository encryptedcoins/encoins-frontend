#!/bin/bash

source ./script/utils.sh

version=$(get_version)
printf "Current frontend version: %s" "$version"

build_dev_js_and_copy "$version"