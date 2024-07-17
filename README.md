# ENCOINS Frontend

## Install GHCJS locally

See instruction [GHCJS.md](GHCJS.md) for manual and dockerized methods.

## General information

- This project is based on cabal 3.2.0.0, GHC 8.6.5 and GHCJS 8.6 versions.
- HLS version for the setup is 1.8.0.0.
- The project is comprised by two packages `frontend` for constructing javascript part and `frontend-html` for constructing html part.

## Building frontend for production

- `build.sh` builds `frontend` and `frontend-html` and copy result to `result` folder.
- `build_and_copy.sh` is spacial version of `build.sh` with deploy preparing.
- `build_js.sh` builds `frontend`.

## Building frontend for development

- `build_html.sh` builds just `frontend-html`.
- `build_js_dev.sh` builds `frontend` in preprod mode

## Launch frontend

```shell
run.sh
```

## Run with caddy server 

Setup [`caddy2`](https://caddyserver.com/v2).

Add `Caddyfile` to root of the frontend project if it doesn't
```
http://localhost:3333 {
  route * {
    root * {$CADDY_ROOT}result
    file_server *
  }
}
```

## In a browser

- `http://localhost:3333/` - landing page
- `http://localhost:3333/app.html` - app page
- `http://localhost:3333/dao.html` - dao page

## Run dockerized ghcjs

1. After building ghcjs-8.6 with docker (see [CHCJS.md](./GHCJS.md)) there is docker image named `ghcjs865`. Check it with `docker images`.

2. Run docker image and share frontend code directory and cabal cache directory (the last one is empty on the first run). Use command

```shell
docker run -it -v <host_path_to_encoins-frontend>:/home/frontend -v <host_path_to_any_empty_directory>:/home/.frontend_cabal_cache ghcjs865 `id -u -n` `id -u`
```

3. Entrypoint of docker is `./start.sh` script which fine tunes infrastructure. It copied on image build and launch automatically.

4. Inside docker run `./build_js_dev.sh` for development and `./build_js.sh` for production. They are wrappers on commands: 
  - build `cabal new-build -f preapp -f predao --ghcjs frontend` and `cabal new-build --ghcjs frontend` respectively.
  - copy js to result directory. 

5. Due to docker's volumes the things built in docker appear on the host.