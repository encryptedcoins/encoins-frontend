#!/bin/bash

cabal new-build -f preapp -f predao --ghcjs frontend
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-website/build/encoins-website/encoins-website.jsexe/all.js result/index.js
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-app/build/encoins-app/encoins-app.jsexe/all.js result/app.js
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-dao/build/encoins-dao/encoins-dao.jsexe/all.js result/dao.js