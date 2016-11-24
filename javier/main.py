# COMP472: Artificial Intelligence
# Javier E. Fajardo - 26487602
# Assignment entry point

import sys
import time
import reversi
from agents import *

def print_usage():
		#FUTURE: maybe add type of game for single or keep-alive
		print("Usage:\n\tpython main.py <Color (B or W)> <Agent> <Agent specific options>")
		print("Known agents:")
		print("\t"+", ".join(list_agents()))
		exit(1)
#end print_usage

def main(args):
	if len(args)<2:
		print_usage()
		
	#args were correct, get the agent we want
	try:
		agent = eval(args[1])
		color = args[0].upper()
		agent_args = args[2:]
	except Exception,e:
		print("An error occurred: "+str(e))
		print_usage()
	
	keep_alive=False
	board_state = ""
	if keep_alive:
		print "Ready to play as %s" % args[0]
	#TODO: Remove when finished
	start = time.time()
	#Game loop
	while(True):
		#current_board = raw_input()  # Read STDIN
		#TODO: Remove when finished
		current_board = '((00000000)(00000000)(00000000)(000WB000)(000BW000)(00000000)(00000000)(00000000))'
		current_board = reversi.parse_board(current_board)
		# agents expect arguments to be passed in as (args, board,symbol)
		move = agent(agent_args, current_board, color)
		#print reversi.check_flip_num(current_board,move,color)
		print "%d %d" % move  # put a move in STDOUT
		
		if not keep_alive or reversi.is_game_over(next_state):
			break
	#end while
	#TODO: Remove when finished
	sys.stderr.write(str(time.time()-start)+"\n")
#end main
	
if __name__=="__main__":
	main(sys.argv[1:])
