# ENCOINS Frontend

## Install GHCJS locally

See instruction [GHCJS.md](GHCJS.md) for mannual and dockerized methods.

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

## Caddy server 

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