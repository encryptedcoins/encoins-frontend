#!/bin/sh

direnv allow .

if [ ! -d "$STORAGE_PATH" ]; then
       echo "Storage disk is not mounted"
       exit 1;
fi;

tmux new-session -d -s "$SESSION" -n "$CARDANO"
tmux split-window -v -t "$SESSION":"$CARDANO".0
tmux split-window -v -t "$SESSION":"$CARDANO".1

tmux send-keys -t "$SESSION":"$CARDANO".0 "cd $SCRIPTS_PATH" C-m;
tmux send-keys -t "$SESSION":"$CARDANO".0 "clear" C-m ;
tmux send-keys -t "$SESSION":"$CARDANO".0 "./node.sh" C-m;

tmux send-keys -t "$SESSION":"$CARDANO".1 "cd $SCRIPTS_PATH" C-m;
tmux send-keys -t "$SESSION":"$CARDANO".1 "clear" C-m;
tmux send-keys -t "$SESSION":"$CARDANO".1 "./kupo.sh" C-m;

tmux send-keys -t "$SESSION":"$CARDANO".2 "cd $SCRIPTS_PATH" C-m;
tmux send-keys -t "$SESSION":"$CARDANO".2 "clear" C-m;
tmux send-keys -t "$SESSION":"$CARDANO".2 "./wallet.sh";


tmux new-window -t "$SESSION":1 -n "$APPS"

tmux split-window -h -t "$SESSION":"$APPS".0
tmux split-window -v -t "$SESSION":"$APPS".0
tmux split-window -v -t "$SESSION":"$APPS".0
tmux split-window -v -t "$SESSION":"$APPS".1

tmux send-keys -t "$SESSION":"$APPS".0 "cd $APPS_PATH" C-m;
tmux send-keys -t "$SESSION":"$APPS".0 "clear" C-m ;
tmux send-keys -t "$SESSION":"$APPS".0 "encoins-delegation" C-m;

tmux send-keys -t "$SESSION":"$APPS".1 "cd $APPS_PATH" C-m;
tmux send-keys -t "$SESSION":"$APPS".1 "clear" C-m ;
tmux send-keys -t "$SESSION":"$APPS".1 "encoins-cloud" C-m;

tmux send-keys -t "$SESSION":"$APPS".2 "cd $APPS_PATH" C-m;
tmux send-keys -t "$SESSION":"$APPS".2 "clear" C-m ;
tmux send-keys -t "$SESSION":"$APPS".2 "encoins --run";

tmux send-keys -t "$SESSION":"$APPS".3 "cd $HOST_FRONT_PATH" C-m;
tmux send-keys -t "$SESSION":"$APPS".3 "clear" C-m ;
tmux send-keys -t "$SESSION":"$APPS".3 "./run.sh ";

tmux send-keys -t "$SESSION":"$APPS".4 "cd $HOST_FRONT_PATH" C-m;
tmux send-keys -t "$SESSION":"$APPS".4 "clear" C-m ;
tmux send-keys -t "$SESSION":"$APPS".4 "docker_run.sh" C-m ;
tmux send-keys -t "$SESSION":"$APPS".4 "./build_js_dev.sh" C-m ;

tmux select-pane -t "$SESSION":"$APPS".2
tmux select-window -t "$SESSION":"$CARDANO".2

# Attach to the session
tmux attach-session -t "$SESSION"