# ENCOINS Frontend

## Prepare environment

1. `mkdir .env`
2. Add env vars to `.env` with format `ENV_NAME=ENV_VALUE`.
If you add all, remove duplicates

Envvar wanted everywhere:
- HOST_ENCOINS - absolute path to directory where all encoins projects sit, that is parent directory to encoins projects.

Envvars for building `ghcjs`:
- DOCKER_CABAL_CACHE=$HOME/.docker_cabal_cache
- DOCKER_FRONTEND=$HOME/frontend 
- GHCJS_IMAGE=ghcjs865
- USER_ID=$(id -u)
- USER_NAME=$(id -u -n)

Envvars for running docker container:
- GHCJS_IMAGE=ghcjs865
- DOCKER_CABAL_CACHE=$HOME/.docker_cabal_cache
- DOCKER_FRONTEND=$HOME/frontend 
- HOST_CABAL_CACHE=$HOST_ENCOINS/.docker_cabal_cache
- HOST_FRONTEND=$HOST_ENCOINS/encoins-frontend

Envvars special for `./script/build_and_copy.sh`
- HOST_WEBSITE=$HOST_ENCOINS/Website
- HOST_WEBAPP=$HOST_ENCOINS/Webapp
- HOST_WEBDAO=$HOST_ENCOINS/DAO
- DOCKER_WEBSITE=$HOME/Website
- DOCKER_WEBAPP=$HOME/Webapp
- DOCKER_WEBDAO=$HOME/DAO

Envvars for development within tmux:
- EXTERNAL_DATA - absolute path to external disk, if you keep data of cardano-node and kupo there (optional)
- HOST_FRONTEND=$HOST_ENCOINS/encoins-frontend
- TOOL_APP="$HOST_ENCOINS/encoins-tools/testnet-preprod/apps/encoins"
- TOOL_SCRIPT="$HOST_ENCOINS/encoins-tools/testnet-preprod/scripts"
- FRONT_SESSION="encoins"
- WINDOW_APPS="apps"
- WINDOW_CARDANO="cardano"

## Install GHCJS locally

See instruction [GHCJS.md](./doc/GHCJS.md) for manual and docker methods.

## General information

- This project is based on cabal 3.2.0.0, GHC 8.6.5 and GHCJS 8.6 versions.
- HLS version for the setup is 1.8.0.0.
- The project is comprised by two packages `frontend` for constructing javascript part and `frontend-html` for constructing html part.

## Building frontend for production

- `./script/build.sh` builds `frontend` and `frontend-html` and copy result to `result` folder.
- `./script/build_and_copy.sh` is spacial version of `./script/build.sh` with deploy preparing. 
- `./script/build_js.sh` builds `frontend` only.
- `./script/build_html.sh` builds `frontend-html` only (the same for prod and dev)

## Building frontend for development

- `./script/build_dev_js.sh` builds `frontend` in preprod mode

## Launch frontend

- Setup [`caddy2`](https://caddyserver.com/v2).

- Add `Caddyfile` to root of the frontend project if it doesn't
```
http://localhost:3333 {
  route * {
    root * {$CADDY_ROOT}result
    file_server *
  }
}
```

- Run frontend
```shell
./script/run.sh
```

## In a browser

- `http://localhost:3333/` - landing page
- `http://localhost:3333/app.html` - app page
- `http://localhost:3333/dao.html` - dao page

## Build and run frontend with docker

1. Check there is docker image named `ghcjs865` with command `docker images` or build it with docker method (see [CHCJS.md](./doc/GHCJS.md)).

2. `script/docker_run.sh` for prod (and `./scrript/docker_dev_run.sh` for dev) run `ghcjs865` container, bind cabal cache and frontend sources (prod version `docker_run.sh` binds Website, Webapp and Webdao as well)

3. Just for info. Entrypoint of ghcjs865 container is `./script/docker_entrypoint.sh` script which fine tunes infrastructure. 

4. Inside docker run `./script/build_dev_js.sh` for dev or `./script/build_js.sh` for prod. 

5. Thanks to docker's volumes the things built in docker appear on the host, that is in result folder.