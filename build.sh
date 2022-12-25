#!/bin/bash

cabal new-build --ghcjs
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-frontend/build/encoins-frontend/encoins-frontend.jsexe/all.js result/
