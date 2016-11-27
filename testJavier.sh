#!/bin/sh
# A quick iteration test against myself
TEST_A="rando"
TEST_B="greedy win"
TEST_C="minimax 4"
#TEST_D="montecarlo" #Not done yet :/

BLACK_TEST=$TEST_A
WHITE_TEST=$TEST_A
cd framework
python main.py "python -u ../javier/main.py W $WHITE_TEST" "python -u ../javier/main.py B $BLACK_TEST"