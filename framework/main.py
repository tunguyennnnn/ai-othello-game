# COMP472: Artificial Intelligence
# Tu Nguyen and Javier E. Fajardo

import sys, os, subprocess, shlex
import time

from othello import OthelloPlay
from player import *
import ui

def PrintUsage():
	print("TJ Othello Engine - Usage:")
	print("  AI v. AI: python main.py \"White Player Commandline\" \"Black Player Commandline\"")
	print("  UI Game : python main.py \"White Player Commandline\"")
	exit(1)
#end PrintUsage

def LaunchUIGame(cpu_player):
	ui.main(cpu_player)
#end 

def LaunchBotGame(black_player,white_player):
	"""
	Manages a game between two AI Agents
	"""
	board = OthelloPlay()
	black_move = (None,None)
	white_move = (None,None)
	pass_move = (-1,-1)
	#TODO: modify and flip the board pieces after each move
	while(black_move!=pass_move and white_move!=pass_move):
		board.printout()
		# black moves first
		black_move = black_player.play(board.serialize())
		print "B: %s" % (str(black_move))
		if black_move!=pass_move:
			board.board[black_move[0]][black_move[1]]=board.black # TODO: make a method in board
		time.sleep(1)
		cereal = board.serialize()

		board.printout()
		# white moves second
		white_move = white_player.play(board.serialize())
		print "W: %s" % (str(white_move))
		if white_move!=pass_move:
			board.board[white_move[0]][white_move[1]]=board.white # TODO: make a method in board
		time.sleep(1)
	#end while
	
	cereal = board.serialize()
	print(cereal)
	print("Game Over")
#end LauchBotGame

def main(args):
	""" Check the arguments and control the flow"""
	#check arguments
	num_args = len(args)
	if num_args==1:
		print("Launching Player v. PC game")
		white = CPU_Player('W',args[0])
		LaunchUIGame(white)
	elif num_args==2:
		print("Launching AI v. AI game")
		white = CPU_Player('W',args[0])
		black = CPU_Player('B',args[1])
		LaunchBotGame(black,white)
	else:
		print("Error with arguments!")
		PrintUsage()


	print "Ending..."
#end main

#entry point
if __name__=="__main__":
	print "Othello engine starting"
	main(sys.argv[1:])