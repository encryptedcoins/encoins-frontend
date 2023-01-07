# ENCOINS Frontend

## Building with Reflex FRP

This project is built with cabal 3.2.0.0 and GHCJS 8.6.5.

In the shell, run
```
cabal new-build --ghcjs
```
to build all webpages. Copy `all.js` files to the respective files in `result/`.

Alternatively, simply run
```
./build.sh
```

## Setup for local development

Switch to cabal 3.2.0.0 and GHC 8.6.5 for HLS support.

Download [`caddy2`](https://caddyserver.com/v2).

Run the server locally `caddy run`.

Open http://localhost:3333/ in the browser to see the results!
