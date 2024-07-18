# Tmux session for development

## Dev up

1. Tmux session uses `docker` to build the project. 
If `ghcjs` installed locally you want to remove following line from `./dev_up.sh`: 
`tmux send-keys -t $FRONT_SESSION:$WINDOW_APPS.4 "./script/docker_dev_run.sh" C-m ;`

2. Update paths variables in `.env` file, see README.md

2. Run `./script/dev_up.sh` script. After launching:
    - wait for sync finishing of `node` and `kupo`
    - run `wallet.sh` in `cardano` window
    - run `encoins --run` in `apps` window to launch relay server
    - run `./script/run.sh` in `apps` window to launch frontend
    - run `./script/build_dev_js.sh` in apps window to rebuild frontend

## Dev down

Just killing session won't stop `node` and `kupo`. 
To gracefully shutting down dev session you want to use following script `./script/dev_down.sh`

