#!/bin/sh
# IMPORTANT NOTE: the framework reads from a processes stdout and writes to stdin
# If you want to incorporate a new program, just have the program flush its stdout buffer
#  after each write.
cd framework
python main.py "python -u ../javier/main.py W minimax 3"
