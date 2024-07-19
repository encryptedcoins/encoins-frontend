#!/bin/sh

set -a
source ./.env
set +a

if [ ! -d "$EXTERNAL_DATA" ]; then
       echo "Storage disk is not mounted"
       exit 1;
fi;

tmux new-session -d -s "$FRONT_SESSION" -n "$WINDOW_CARDANO"
tmux split-window -v -t "$FRONT_SESSION":"$WINDOW_CARDANO".0
tmux split-window -v -t "$FRONT_SESSION":"$WINDOW_CARDANO".1

tmux send-keys -t "$FRONT_SESSION":"$WINDOW_CARDANO".0 "cd $TOOL_SCRIPT" C-m;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_CARDANO".0 "clear" C-m ;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_CARDANO".0 "./node.sh" C-m;

tmux send-keys -t "$FRONT_SESSION":"$WINDOW_CARDANO".1 "cd $TOOL_SCRIPT" C-m;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_CARDANO".1 "clear" C-m;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_CARDANO".1 "./kupo.sh" C-m;

tmux send-keys -t "$FRONT_SESSION":"$WINDOW_CARDANO".2 "cd $TOOL_SCRIPT" C-m;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_CARDANO".2 "clear" C-m;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_CARDANO".2 "./wallet.sh";


tmux new-window -t "$FRONT_SESSION":1 -n "$WINDOW_APPS"

tmux split-window -h -t "$FRONT_SESSION":"$WINDOW_APPS".0
tmux split-window -v -t "$FRONT_SESSION":"$WINDOW_APPS".0
tmux split-window -v -t "$FRONT_SESSION":"$WINDOW_APPS".1

tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".0 "cd $TOOL_APP" C-m;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".0 "clear" C-m ;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".0 "encoins-delegation" C-m;

tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".1 "cd $TOOL_APP" C-m;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".1 "clear" C-m ;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".1 "encoins-cloud" C-m;

tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".2 "cd $HOST_FRONTEND" C-m;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".2 "clear" C-m ;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".2 "./script/run.sh ";

tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".3 "cd $TOOL_APP" C-m;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".3 "clear" C-m ;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".3 "encoins --run";

tmux select-pane -t "$FRONT_SESSION":"$WINDOW_APPS".3
tmux select-window -t "$FRONT_SESSION":"$WINDOW_CARDANO".2

# Attach to the session
tmux attach-session -t "$FRONT_SESSION"