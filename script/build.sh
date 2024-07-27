#!/bin/bash

source ./script/common.sh

version=$(get_version)
printf "Current frontend version: %s" "$version"

build_prod_js_html_and_copy "$version"
