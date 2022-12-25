# ENCOINS Basic Frontend

## Building Reflex frontend

This project is built with cabal 3.2.0.0 and GHCJS 8.6.5.

In the shell, run
```
cabal new-build --ghcjs
```
to build it.

Copy `all.js` file to `result/`:

```
cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/encoins-frontend-0.1.0.0/x/encoins-frontend/build/encoins-frontend/encoins-frontend.jsexe/all.js result/
```

## Setup for local development

Switch to cabal 3.2.0.0 and GHC 8.6.5 for HLS support.

Download [`caddy2`](https://caddyserver.com/v2).

Run the server locally `caddy run`.

Open http://localhost:3333/ in the browser to see the results!
