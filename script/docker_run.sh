#!/bin/bash

# set -eux -o pipefail

set -a
source ./.env
set +a

mkdir -p "$HOST_CABAL_CACHE"
mkdir -p "$HOST_WEBSITE"
mkdir -p "$HOST_WEBAPP"
mkdir -p "$HOST_WEBDAO"

docker run -it -v "$HOST_FRONTEND":"$DOCKER_FRONTEND" \
    -v "$HOST_CABAL_CACHE":"$DOCKER_CABAL_CACHE" \
    -v "$HOST_WEBSITE":"$DOCKER_WEBSITE" \
    -v "$HOST_WEBAPP":"$DOCKER_WEBAPP" \
    -v "$HOST_WEBDAO":"$DOCKER_WEBDAO" \
    "$GHCJS_IMAGE"