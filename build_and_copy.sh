#!/bin/bash

cabal new-build --ghcjs
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-website/build/encoins-website/encoins-website.jsexe/all.js result/index.js
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-app/build/encoins-app/encoins-app.jsexe/all.js result/app.js

export WEBSITE_DIR=../Website
cp -r -T result $WEBSITE_DIR
rm $WEBSITE_DIR/app.html
rm $WEBSITE_DIR/app.js

export WEBAPP_DIR=../Webapp
cp -r -T result $WEBAPP_DIR
rm $WEBAPP_DIR/index.html
rm $WEBAPP_DIR/index.js
mv $WEBAPP_DIR/app.html $WEBAPP_DIR/index.html
rm -r $WEBAPP_DIR/ispo
rm -r $WEBAPP_DIR/docs