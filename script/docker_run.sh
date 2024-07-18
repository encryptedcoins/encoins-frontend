#!/bin/bash

# set -eux -o pipefail

set -a
source ./.env
set +a

mkdir -p "$HOST_CABAL_CACHE"

docker run -it -v "$HOST_FRONT_PATH":"$DOCKER_FRONT_PATH" \
    -v "$HOST_CABAL_CACHE":"$DOCKER_CABAL_CACHE" \
    "$GHCJS_IMAGE"