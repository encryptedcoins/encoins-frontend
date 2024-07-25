#!/bin/bash

source ./script/common.sh

version=$(get_version)
printf "Current frontend version: %s" "$version"

build_dev_js_and_copy "$version"