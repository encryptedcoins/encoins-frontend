#!/bin/bash

# set -eux -o pipefail

# Extract version of frontend from cabal
get_version() {
  version=$(grep -P "^version:" frontend/encoins-frontend.cabal | awk '{print $2}')
  if [ -z "$version" ]; then
    echo "Error: cound not extract value of version from encoins-frontend.cabal"
    exit 1
  fi
  echo "$version"
}

build_html() {
  cabal run --project-file=frontend-html.project frontend-html
}

build_prod() {
  cabal new-build --ghcjs frontend
}

build_dev() {
  cabal new-build -f preapp -f predao --ghcjs frontend
}

copy_main() {
  cp "dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-${1}/x/encoins-website/build/encoins-website/encoins-website.jsexe/all.js" "result/index.js"
}

copy_app() {
  cp "dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-${1}/x/encoins-app/build/encoins-app/encoins-app.jsexe/all.js" "result/app.js"
}

copy_dao() {
  cp "dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-${1}/x/encoins-dao/build/encoins-dao/encoins-dao.jsexe/all.js" "result/dao.js"
}

copy_js() {
  printf '\n\n==== Copy js files to result ===='
  if ! copy_main "$1"; then
    printf "\n\nCoping main page is failed."
  else
    if ! copy_app "$1"; then
      printf "\n\nCoping app is failed"
    else
      if ! copy_dao "$1"; then
        printf "\n\nCoping dao is failed."
      else
        printf "\nCoping is successful!\n\n"
      fi
    fi
  fi
}

build_prod_js_and_copy() {
  printf '\n==== Build js frontend for production ====\n'
  if ! build_prod; then
    printf "\n\nBuilding is failed."
  else
    printf "Prod is built successfully!"
    copy_js "$1"
  fi
}

build_prod_js_html_and_copy() {
  printf '\n\n==== Build HTML frontend ====\n'
  if ! build_html; then
    printf "Building HTML is failed."
  else
    build_prod_js_and_copy "$version"
  fi
}
