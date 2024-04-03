# ENCOINS Frontend

## Install GHCJS locally

See instruction [GHCJS.md](GHCJS.md)

## Building

- This project is based on cabal 3.2.0.0, GHC 8.6.5 and GHCJS 8.6 versions.
- The project is comprised by two packages `frontend` for constructing javascript part and `frontend-html` for constructing html part.
- For building both of them at once just run `build.sh` that builds them and copy results to `result` folder.

## Development

1. For building just the `frontend-html` part of the project use commands:

```shell
cabal run --project-file=frontend-html.project frontend-html
```
They generate html files to `result/` folder.

2. Building `frontend` part of the project for mainnet use commands:

```shell
cabal new-build --ghcjs frontend
```
to build all webpages. Copy `all.js` files to the respective files in `result/`.

Alternatively, simply run

```shell
./build-frontend.sh
```
It will compile frontend and copy required files to `result/`.

1. Building frontend for preprod use flags `predao` and `preapp`

```shell
cabal new-build -f preapp -f predao --ghcjs frontend
```
or simply run

```shell
./build-frontend-dev.sh
```
It will compile frontend in dev mode and copy required files to `result/`.

4. Rebuild frontend in dev mode and run it with caddy at once

```shell
run-dev.sh
```

## Setup for local development

Switch to cabal 3.2.0.0 and GHC 8.6.5 for HLS support (HLS version for the setup is 1.8.0.0).

Download [`caddy2`](https://caddyserver.com/v2).

Run the server locally `caddy run`.

Open http://localhost:3333/ or http://localhost:3333/app.html or http://localhost:3333/dao.html in the browser to see the results!
