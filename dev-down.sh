#!/bin/sh

direnv allow .

tmux send-keys -t "$SESSION":"$CARDANO".0 C-c ;
tmux send-keys -t "$SESSION":"$CARDANO".1 C-c ;
tmux send-keys -t "$SESSION":"$CARDANO".2 C-c ;

tmux send-keys -t "$SESSION":"$APPS".0 C-c ;
tmux send-keys -t "$SESSION":"$APPS".1 C-c ;
tmux send-keys -t "$SESSION":"$APPS".2 C-c ;
tmux send-keys -t "$SESSION":"$APPS".3 C-c ;

tmux kill-session -t "$SESSION"