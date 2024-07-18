#!/bin/bash

# set -eux -o pipefail

set -a
source ./.env
set +a

mkdir -p "$HOST_CABAL_CACHE"

docker run -it -v "$HOST_FRONTEND":"$DOCKER_FRONTEND" \
    -v "$HOST_CABAL_CACHE":"$DOCKER_CABAL_CACHE" \
    "$GHCJS_IMAGE"
