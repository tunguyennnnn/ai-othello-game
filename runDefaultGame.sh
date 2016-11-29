#!/bin/sh
# IMPORTANT NOTE: the framework reads from a processes stdout and writes to stdin
# If you want to incorporate a new program, just have the program flush its stdout buffer
#  after each write.
cd framework
PLAYER_A="python -u ../javier/main.py B minimax 3 NoCorners"
PLAYER_B="clisp ../tu/tu-othello.cl W 3"

python main.py "$PLAYER_B" "$PLAYER_A"
