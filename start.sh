#!/bin/bash

export USER_NAME=$1
export USER_ID=$2

echo "configuring user: $USER_NAME ..."

sudo adduser --disabled-password --gecos '' --uid $USER_ID $USER_NAME > /dev/null 2>&1 
sudo adduser $USER_NAME sudo > /dev/null 2>&1 

if [ -z "$( ls -A '/home/.frontend_cabal_cache' )" ]; then
    printf "\nFirst run. Preparing infrastructure...\n\n"
    id -u -n
    id -u 
    cabal update
    pwd
    cabal install alex-3.2.6
    cabal install happy-1.19.9
fi

WORKAREA=/home/frontend
cd $WORKAREA

bash