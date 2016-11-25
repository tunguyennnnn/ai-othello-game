# COMP472: Artificial Intelligence
# Tu Nguyen and Javier E. Fajardo

import sys, os, subprocess, shlex
import subprocess as sb

from othello import OthelloPlay
from player import *

def PrintUsage():
	print("TJ Othello Engine - Usage:")
	print("  AI v. AI: python main.py \"Black Player Commandline\" \"White Player Commandline\"")
	print("  UI Game : python main.py \"Black Player Commandline\"")
	exit(1)
#end PrintUsage

def LaunchUIGame(cpu_player):
	raise NotImplemented("Need to integrate with the UI first")
#end 

def LaunchBotGame(black_player,white_player):
	"""
	Manages a game between two AI Agents
	"""
	board = OthelloPlay()
	black_move = (None,None)
	white_move = (None,None)
	pass_move = (-1,-1)
	
	while(black_move!=pass_move and white_move!=pass_move):
		cereal = board.serialize()
		print(cereal)
		# black moves first
		black_move = black_player.play(cereal)
		if black_move!=pass_move:
			board.board[black_move[0]][black_move[1]]=board.black # TODO: make a method in board
		
		cereal = board.serialize()
		print(cereal)
		# white moves second
		white_move = black_player.play(cereal)
		if white_move!=pass_move:
			board.board[black_move[0]][black_move[1]]=board.black # TODO: make a method in board
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
		LaunchUIGames(white)
	elif num_args==2:
		print("Launching AI v. AI game")
		black = CPU_Player('B',args[0])
		white = CPU_Player('W',args[1])
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