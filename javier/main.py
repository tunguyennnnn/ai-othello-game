# COMP472: Artificial Intelligence
# Javier E. Fajardo - 26487602
# Assignment entry point

import sys
import reversi
from agents import *

def main(args):
	if len(args)<1:
		#FUTURE: maybe add type of game for single or keep-alive
		print("Usage:\n\tpython main.py <Agent> <Agent specific options>")
		print("Known agents:")
		#print("\t %s" % [x for x in dir(agents) if x.find('_')<0])
		print("\t"+", ".join(list_agents()))
		exit(1)
		
	#args were correct, get the agent we want
	agent = eval(args[0])
	agent_args = args[1:]
	
	keep_alive=False
	board_state = ""
	if keep_alive:
		print "Ready to play as %s" % args[0]
	
	#Game loop
	while(True):
		current_board = raw_input()  # Read STDIN
		current_board = reversi.parse_board(current_board)
		
		next_state = agent(current_board, agent_args)
		print(next_state)  # put it back to STDOUT
		
		if not keep_alive or reversi.is_game_over(next_state):
			break
	#end while
#end main
	
if __name__=="__main__":
	main(sys.argv[1:])