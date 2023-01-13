#!/bin/bash

cabal new-build --ghcjs
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-website/build/encoins-website/encoins-website.jsexe/all.js result/index.js
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-app/build/encoins-app/encoins-app.jsexe/all.js result/app.js