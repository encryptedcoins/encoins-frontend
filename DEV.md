# Tmux session for development

## Dev up

1. Create script `dev-up.sh` anywhere (it use absolute paths anyway)
2. Insert the following stuff to the script file: 

```shell
#!/bin/sh

SESSION="encoins"
APPS="apps"
CARDANO="cardano"

# update path variables to match your directories
ENCOINS_PATH="source/org/encoins" 
APPS_PATH="$HOME/$ENCOINS_PATH/encoins-tools/testnet-preprod/apps/encoins"
SCRIPTS_PATH="$HOME/$ENCOINS_PATH/encoins-tools/testnet-preprod/scripts"
FRONT_PATH="$HOME/$ENCOINS_PATH/encoins-frontend"

tmux new-session -d -s $SESSION -n $CARDANO
tmux split-window -v -t $SESSION:$CARDANO.0
tmux split-window -v -t $SESSION:$CARDANO.1

tmux send-keys -t $SESSION:$CARDANO.0 "cd $SCRIPTS_PATH" C-m;
tmux send-keys -t $SESSION:$CARDANO.0 "clear" C-m ;
tmux send-keys -t $SESSION:$CARDANO.0 "./node.sh" C-m;

tmux send-keys -t $SESSION:$CARDANO.1 "cd $SCRIPTS_PATH" C-m;
tmux send-keys -t $SESSION:$CARDANO.1 "clear" C-m;
tmux send-keys -t $SESSION:$CARDANO.1 "./kupo.sh" C-m;

tmux send-keys -t $SESSION:$CARDANO.2 "cd $SCRIPTS_PATH" C-m;
tmux send-keys -t $SESSION:$CARDANO.2 "clear" C-m;
tmux send-keys -t $SESSION:$CARDANO.2 "./wallet.sh";


tmux new-window -t $SESSION:1 -n $APPS

tmux split-window -h -t $SESSION:$APPS.0
tmux split-window -v -t $SESSION:$APPS.0
tmux split-window -v -t $SESSION:$APPS.1

tmux send-keys -t $SESSION:$APPS.0 "cd $APPS_PATH" C-m;
tmux send-keys -t $SESSION:$APPS.0 "clear" C-m ;
tmux send-keys -t $SESSION:$APPS.0 "encoins-delegation" C-m;

tmux send-keys -t $SESSION:$APPS.1 "cd $APPS_PATH" C-m;
tmux send-keys -t $SESSION:$APPS.1 "clear" C-m ;
tmux send-keys -t $SESSION:$APPS.1 "encoins-cloud" C-m;

tmux send-keys -t $SESSION:$APPS.2 "cd $APPS_PATH" C-m;
tmux send-keys -t $SESSION:$APPS.2 "clear" C-m ;
tmux send-keys -t $SESSION:$APPS.2 "encoins --run";

tmux send-keys -t $SESSION:$APPS.3 "cd $FRONT_PATH" C-m;
tmux send-keys -t $SESSION:$APPS.3 "clear" C-m ;
tmux send-keys -t $SESSION:$APPS.3 "./run_dev.sh ";

tmux select-pane -t $SESSION:$CARDANO.2
tmux select-pane -t $SESSION:$APPS.2

# Attach to the session
tmux attach-session -t $SESSION
```

3. Update paths variables to match your directories

4. After running `dev-up.sh` 
    - wait for sync finishing of `node` and `kupo`
    - run `wallet.sh` in `cardano` window
    - run `encoins --run` in `apps` window to launch relay server
    - run `run-dev.sh` in `apps` window to launch frontend

## Dev down

Just killing session won't stop `node` and `kupo`. 
To gracely shutting down dev session you want to use followng script:

```shell
#!/bin/sh

SESSION="encoins"
APPS="apps"
CARDANO="cardano"

tmux send-keys -t $SESSION:$CARDANO.0 C-c ;
tmux send-keys -t $SESSION:$CARDANO.1 C-c ;
tmux send-keys -t $SESSION:$CARDANO.2 C-c ;

tmux send-keys -t $SESSION:$APPS.0 C-c ;
tmux send-keys -t $SESSION:$APPS.1 C-c ;
tmux send-keys -t $SESSION:$APPS.2 C-c ;
tmux send-keys -t $SESSION:$APPS.3 C-c ;

tmux kill-session -t $SESSION
```

