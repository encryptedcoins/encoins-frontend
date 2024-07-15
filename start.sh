#!/bin/bash

export USER_NAME=$1
export USER_ID=$2

echo "configuring user: $USER_NAME ..."

sudo adduser --disabled-password --gecos '' --uid $USER_ID $USER_NAME > /dev/null 2>&1 
sudo adduser $USER_NAME sudo > /dev/null 2>&1 

WORKAREA=/home/frontend
cd $WORKAREA

bash