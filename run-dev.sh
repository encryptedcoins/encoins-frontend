#!/bin/bash

# Execute the script file
# ./build-frontend-dev.sh
cabal new-build -f preapp -f predao --ghcjs frontend

# Check the exit status using $?
if [ $? -eq 0 ]; then
    echo "Building frontend was successful."
    cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-website/build/encoins-website/encoins-website.jsexe/all.js result/index.js
    if [ $? -eq 0 ]; then
      echo "Copping main page was successful."
      cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-app/build/encoins-app/encoins-app.jsexe/all.js result/app.js
      if [ $? -eq 0 ]; then
        echo "Copping app page was successful."
        cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-dao/build/encoins-dao/encoins-dao.jsexe/all.js result/dao.js
        if [ $? -eq 0 ]; then
          echo "Copping dao page was successful."
          caddy run
        else
          echo ""
          echo "Caddy returned an error."
        fi
      else
        echo ""
        echo "Coping app returned an error."
      fi
    else
      echo ""
      echo "Coping app returned an error."
    fi
else
    echo ""
    echo "Building frontend returned an error."
fi
