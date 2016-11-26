# COMP472: Artificial Intelligence
# Tu Nguyen and Javier E. Fajardo

import sys, os, subprocess, shlex
import time

from othello import OthelloPlay
from player import *
from ui import *
import Tkinter as tk

def PrintUsage():
	print("TJ Othello Engine - Usage:")
	print("  AI v. AI: python main.py \"White Player Commandline\" \"Black Player Commandline\"")
	print("  UI Game : python main.py \"White Player Commandline\"")
	exit(1)
#end PrintUsage

def LaunchUIGame(cpu_player):
	ui.main(cpu_player)
#end 

def LaunchBotGame(black_player,white_player, tkroot, uiboard):
	"""
	Manages a game between two AI Agents
	"""
	board = OthelloPlay()
	black_move = (None,None)
	white_move = (None,None)
	pass_move = (-1,-1)
	checkupdate(tkroot, uiboard, board.board)
	#TODO: modify and flip the board pieces after each move
	while(black_move!=pass_move and white_move!=pass_move):
		board.printout()
		# black moves first
		black_move = black_player.play(board.serialize())
		print "B: %s" % (str(black_move))
		if black_move!=pass_move:
			(row, col) = black_move
			board.add_to_board(board.black, row, col)   # TODO: make a method in board
			checkupdate(tkroot, uiboard, board.board)
			board.printout()
		time.sleep(1)
		cereal = board.serialize()

		# white moves second
		white_move = white_player.play(board.serialize())
		print "W: %s" % (str(white_move))
		if white_move!=pass_move:
			(row, col) = white_move
			board.add_to_board(board.white, row, col) # TODO: make a method in board
			checkupdate(tkroot, uiboard, board.board)
			board.printout()
		time.sleep(1)
	#end while
	
	cereal = board.serialize()
	print(cereal)
	print("Game Over")
#end LauchBotGame

def checkupdate(root, board, play_board):
	print play_board
	black_count = white_count = 0;
	board.canvas.delete("piece")
	for (row, sublist) in zip(range(8), play_board):
		for (col, player_type) in zip(range(8), sublist):
			if player_type == "B":
				black_count += 1
				board.addBlack(col, row)
			elif player_type == "W":
				white_count += 1
				board.addWhite(col, row)
	board.currentStatus("B", black_count, white_count)
	root.update()

#call back: after the ui board is generated

def main(args):
	""" Check the arguments and control the flow"""
	#check arguments
	num_args = len(args)

	# generate ui config:
	root = tk.Tk()
	uiboard = None
	if num_args==1:
		print("Launching Player v. PC game")
		white = CPU_Player('W',args[0])
		uiboard = GameBoard(root, True)
		uiboard.pack(side="top", fill="both", expand="true", padx=4, pady=4)
		uiboard.currentStatus("black", 0, 0)
		LaunchUIGame(white)
	elif num_args==2:
		print("Launching AI v. AI game")
		white = CPU_Player('W',args[0])
		black = CPU_Player('B',args[1])
		uiboard = GameBoard(root)
		uiboard.pack(side="top", fill="both", expand="true", padx=4, pady=4)
		uiboard.currentStatus("black", 0, 0)
		root.after(1000, LaunchBotGame, black, white, root, uiboard)
	else:
		print("Error with arguments!")
		PrintUsage()

	#start the game
	if uiboard:
		root.mainloop()
	print "Ending..."
#end main

#entry point
if __name__=="__main__":
	print "Othello engine starting"
	main(sys.argv[1:])