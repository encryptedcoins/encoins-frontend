#!/bin/bash

export USER_NAME=$(id -u -n)
export USER_ID=$(id -u)

printf "\nUSER_NAME: %s" $USER_NAME
printf "\nUSER_ID: %s" $USER_ID
printf "\npwd: %s\n\n" $(pwd)

printf "\nConfiguring user: %s ..." $USER_NAME

sudo adduser --disabled-password --gecos '' --uid $USER_ID $USER_NAME > /dev/null 2>&1 
sudo adduser $USER_NAME sudo > /dev/null 2>&1 

ls -A /home/$USER_NAME/.frontend_cabal_cache
if [ ! -d /home/$USER_NAME/.frontend_cabal_cache ] || [ -z "$(ls -A /home/$USER_NAME/.frontend_cabal_cache )" ]; then
    printf "\nFirst run. Preparing infrastructure...\n\n"
    cabal update
    cabal install alex-3.2.6
    cabal install happy-1.19.9
fi

cd $HOME/frontend

bash