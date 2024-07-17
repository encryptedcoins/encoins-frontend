#!/bin/bash

export USER_NAME=$(id -u -n)
export USER_ID=$(id -u)

printf "\nConfiguring user: %s ...\n" $USER_NAME

sudo adduser --disabled-password --gecos '' --uid $USER_ID $USER_NAME > /dev/null 2>&1 
sudo adduser $USER_NAME sudo > /dev/null 2>&1 

if [ ! -d $HOME/.docker_cabal_cache ] || [ -z "$(ls -A $HOME/.docker_cabal_cache)" ]; then
    printf "\nFirst run. Preparing infrastructure..."
    printf "\n\nUSER_NAME: %s" $USER_NAME
    printf "\nUSER_ID: %s" $USER_ID
    printf "\npwd: %s\n\n" $(pwd)
    cabal update
    cabal install alex-3.2.6
    cabal install happy-1.19.9
fi

cd $HOME/frontend

bash