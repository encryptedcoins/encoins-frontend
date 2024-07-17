# Tmux session for development

## Dev up

1. Tmux session uses `docker` to build the project. 
If `ghcjs` installed locally you want to remove following line from `./dev-up.sh`: 
`tmux send-keys -t $SESSION:$APPS.4 "docker_run.sh" C-m ;`

2. Update paths variables in `.envrc` file

2. Run `dev-up.sh` script. After launching:
    - wait for sync finishing of `node` and `kupo`
    - run `wallet.sh` in `cardano` window
    - run `encoins --run` in `apps` window to launch relay server
    - run `run-dev.sh` in `apps` window to launch frontend

## Dev down

Just killing session won't stop `node` and `kupo`. 
To gracefully shutting down dev session you want to use following script `./dev-down.sh`

