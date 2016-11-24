# COMP472: Artificial Intelligence
# Javier E. Fajardo - 26487602
# agents.py: defines all agents made to play Reversi for one or several moves

import heuristics
import reversi
import reversi_utils

#TODO: EXTENSIVELY TEST
# Right now, this is about as good as pseudocode
# Mainly, decide on the format being operated on. Certain places assume strings. Others have the ".value" to retrieve a string. this should be uniform

def list_agents():
	import inspect,sys
	functions = inspect.getmembers(sys.modules[__name__], inspect.isfunction)
	sanitized = [x[0] for x in functions if x[0]!="list_agents"]
	return sanitized
#end list_agents

def echo_agent(args, board, symbol):
	print(board,symbol)
	return reversi.valid_moves(board,symbol)
#end echo_agent

def rando(args, board,symbol):
	"""
	A Random Agent that plays wherever it can play at random points in time
	"""
	#TODO: 
	return None
#end rando

def greedy(args, board, symbol):
	"""
	A regular greedy agent which maximizes the amount of pieces flipped (always)
	"""
	# no args expected
	heuristic = MaxPieces
	if len(args)>1:
		heuristic = eval("heuristics."+args[0])

	#TODO: are we operating on a list of strings or a list of nodes?
	subnodes = reversi.valid_moves(root.value,maxSymbol)
	best_index=-1
	best_value=-1
	for i in range(len(subnodes)):
		val=reversi.check_flip_num(board,subnodes[i],symbol)
		if(val>best_value):
			best_value=val
			best_index=-1
	#end for
	move = subnodes[best_index]
	board[move[0]][move[1]]=symbol

	return board
#end greedy

def minimax(args, board, symbol):
	#One-time runtime check
	if(len(args)<1):
		raise RuntimeError("Minimax agent expected a list with at least the depth!")

	in_depth = args[0]
	heuristic = heuristics.MaxPieces
	if(len(args)>1):
		heuristic = eval("heuristics."+args[1])

	# Some simple method definitions
	counterpart = reversi_utils.get_counterpart
	#TODO: add heuristic evaluation in recursive method
	#TODO: make board a node 
	def recursive(root,depth,maxSymbol):	
		"""Recursive component of minimax"""
		if depth<1:
			return root
		
		#adjust for either maximizing or minimizing
		evaluator = max
		turnValue = -2**32
		nextSymbol = maxSymbol
		if maxSymbol!=symbol:
			evaluator=min
			turnValue = 2**32
			nextSymbol=counterpart(maxSymbol)
		
		# Get all nodes and evaluate them
		#TODO: are we operating on a list of strings or a list of nodes?
		subnodes = reversi.valid_moves(root.value,maxSymbol)
		bestPick = (turnValue,None)
		for node in subnodes:
			return_node = recursive(node,depth-1,nextSymbol)
			value = evaluator(bestPick[0],return_node.value)
			if(value!=bestPick[0]):
				bestPick = (value,return_node)
		#end for
		return_node = bestPick[1]
		return_node.value = bestPick[0]
		
		return return_node
	#end recursive
	
	#the actual work
	return recursive(board,in_depth,symbol)
#end minimax
