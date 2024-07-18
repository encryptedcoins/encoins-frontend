#!/bin/bash

# $USER, $UID and $HOME env vars taken from ENVs of dockerfile where this script starts

printf "\nConfiguring user: %s ...\n" "$USER"

sudo adduser --disabled-password --gecos '' --uid "$UID" "$USER" > /dev/null 2>&1 
sudo adduser "$USER" sudo > /dev/null 2>&1 

if [ ! -d "$HOME"/.docker_cabal_cache ] || [ -z "$(ls -A "$HOME"/.docker_cabal_cache)" ]; then
    printf "\nFirst run. Preparing infrastructure..."
    printf "\n\nUSER: %s" "$USER"
    printf "\nUID: %s" "$UID"
    printf "\npwd: %s\n\n" "$(pwd)"
    cabal update
    cabal install alex-3.2.6
    cabal install happy-1.19.9
fi

cd "$HOME"/frontend || exit

bash