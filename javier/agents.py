# COMP472: Artificial Intelligence
# Javier E. Fajardo - 26487602
# agents.py: defines all agents made to play Reversi for one or several moves

import random
import reversi
import heuristics

# NOTE: if the agent doesn't find a valid move and is called, it will return (-1,-1)

def list_agents():
	import inspect,sys
	functions = inspect.getmembers(sys.modules[__name__], inspect.isfunction)
	sanitized = [x[0] for x in functions if x[0]!="list_agents"]
	return sanitized
#end list_agents

def echo_agent(args, board, symbol):
	"""
	Just prints and echoes back. Used for debug purposes
	"""
	print(board,symbol)
	
	moves = reversi.valid_moves(board,symbol)
	print(heuristics.Win(board,moves,symbol))
	ret = (3,4)
	return ret
#end echo_agent

def rando(args, board,symbol):
	"""
	A Random Agent that plays wherever it can play at random points in time
	"""
	valid_next_moves = reversi.valid_moves(board,symbol)
	if len(valid_next_moves)<1:
		return (-1,-1)
	return random.choice(valid_next_moves)
#end rando

def greedy(args, board, symbol):
	"""
	A regular greedy agent which maximizes the amount of pieces flipped (always)
	"""
	# no args expected
	heuristic = heuristics.MaxPieces
	if len(args)>1:
		heuristic = eval("heuristics."+args[0])

	# We are operating on tuples
	possible_moves = reversi.valid_moves(board,symbol)
	if len(possible_moves)<1:
		return (-1,-1)
		
	results = heuristic(board, possible_moves, symbol)
	results.sort()
	results.reverse()

	return results[0][1]
#end greedy

def minimax(args, board, symbol):
	"""
	TODO: Complete the minimax function. This will not work as it is
	"""
	# One-time runtime check
	if(len(args)<1):
		raise RuntimeError("Minimax agent expected a list with at least the depth!")

	in_depth = args[0]

	evaluator = heuristics.MaxPieces
	if(len(args)>1):
		evaluator = eval("heuristics."+args[1])
	# Finished checks and setup

	def mm_recursive(start,depth,maxSymbol):	
		"""Recursive component of minimax"""
		is_other_player = maxSymbol!=symbol
		moves = reversi.valid_moves(start.value,maxSymbol)
		
		# Out of depth
		if depth<1:
			# deal with end games
			if len(moves)<1:
				if is_other_player:
					return -2**32
				else:
					return 2**32
			# end if
			
			moves_eval = evaluator(start.value,moves,maxSymbol)
			moves_eval.sort()
			root.add_lazy_edges(moves_eval,is_other_player)
			update_val = move_eval[-1][0]
			if is_other_player:
				update_val = -update_val
			if start.root!=None:
				start.root.heuristic=update_val
			return update_val
		#end if depth
		
		#adjust for either maximizing or minimizing
		turn_val = -2**32
		nextSymbol = maxSymbol
		if is_other_player:
			turn_val = 2**32
			nextSymbol=counterpart(maxSymbol)
		
		# Get all nodes and evaluate them
		update_val = 0
		for a_move in moves:
			# Play the move, make a new board, call recursively
			next_board = reversi.play_move(start.value,a_move)
			next_node = node(start,next_board,0)
			start.add_child(next_node,0,a_move)
			
			next_val = mm_recursive(next_node,depth-1,nextSymbol)
			start.edges[-1].weight = next_val
			next_node.heuristic = next_val
			
			if is_other_player:
				if next_val < turn_val:
					turn_val = next_val
			else:
				if next_val > turn_val:
					turn_val = next_val
		#end for
		if start.root!=None:
			start.root.heuristic = turn_val	
		return turn_val
	#end recursive

	# the actual work
	#TODO: test. There might be an issue where the best value is actually in the node and not the edge
	root_node = node(None,board,0)	
	best_val = mm_recursive(root_node,in_depth,symbol)
	edge_list = root.edges
	for edge in edge_list:
		if edge.end_node.heuristic==best_val:
			return edge.label
	

	return (-1,-1)
#end minimax
