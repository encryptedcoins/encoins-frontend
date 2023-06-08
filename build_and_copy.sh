#!/bin/bash

cabal new-build --ghcjs
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-website/build/encoins-website/encoins-website.jsexe/all.js result/index.js
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-app/build/encoins-app/encoins-app.jsexe/all.js result/app.js
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-dao/build/encoins-dao/encoins-dao.jsexe/all.js result/dao.js

export WEBSITE_DIR=../Website
cp -r -T result $WEBSITE_DIR
rm -f $WEBSITE_DIR/app.html
rm -f $WEBSITE_DIR/app.js
rm -f $WEBSITE_DIR/dao.html
rm -f $WEBSITE_DIR/dao.js

export WEBAPP_DIR=../Webapp
cp -r -T result $WEBAPP_DIR
rm -f $WEBAPP_DIR/index.html
rm -f $WEBAPP_DIR/index.js
rm -f $WEBAPP_DIR/dao.html
rm -f $WEBAPP_DIR/dao.js
mv $WEBAPP_DIR/app.html $WEBAPP_DIR/index.html
rm -r $WEBAPP_DIR/ispo
rm -r $WEBAPP_DIR/docs

export DAO_DIR=../DAO
cp -r -T result $DAO_DIR
rm -f $DAO_DIR/index.html
rm -f $DAO_DIR/index.js
rm -f $DAO_DIR/app.html
rm -f $DAO_DIR/app.js
mv $DAO_DIR/dao.html $DAO_DIR/index.html
rm -r $DAO_DIR/ispo
rm -r $DAO_DIR/docs