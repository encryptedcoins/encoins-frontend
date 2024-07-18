#!/bin/sh

set -a
source ./.env
set +a

tmux send-keys -t "$FRONT_SESSION":"$WINDOW_CARDANO".0 C-c ;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_CARDANO".1 C-c ;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_CARDANO".2 C-c ;

tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".0 C-c ;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".1 C-c ;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".2 C-c ;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".3 C-c ;
tmux send-keys -t "$FRONT_SESSION":"$WINDOW_APPS".4 C-c ;

tmux kill-session -t "$FRONT_SESSION"