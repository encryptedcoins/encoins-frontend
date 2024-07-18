#!/bin/bash

set -a
source ./.env
set +a

docker build --progress=plain -t "$GHCJS_IMAGE" \
    --build-arg uid="$USER_ID" \
    --build-arg username="$USER_NAME" \
    --build-arg docker_cabal_cache="$DOCKER_CABAL_CACHE" \
    --build-arg docker_frontend="$DOCKER_FRONTEND" \
    --build-arg docker_website="$DOCKER_WEBSITE" \
    --build-arg docker_webapp="$DOCKER_WEBAPP" \
    --build-arg docker_webdao="$DOCKER_WEBDAO" .