#!/bin/bash

direnv allow .envrc

docker build --progress=plain -t "$GHCJS_IMAGE" \
    --build-arg uid="$USER_ID" \
    --build-arg username="$USER_NAME" \
    --build-arg docker_cabal_cache="$DOCKER_CABAL_CACHE" \
    --build-arg docker_front_path="$DOCKER_FRONT_PATH" .