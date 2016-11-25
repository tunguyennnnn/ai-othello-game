# COMP472: Artificial Intelligence
# Javier E. Fajardo - 26487602
# heuristics.py: defines all heuristics that can be used in weighing decisions in a game algorithm

from reversi import check_flip_num

def MaxPieces(board, moves, color):
	"""
	Counts the number of pieces flipped by each move and returns a list of tuples
	Return format: [(value,(x_pos,y_pos)),(value,(x_pos,y_pos)),...]
	"""
	retVal = []
	evaluator = lambda x: check_flip_num(board,x,color)
	return [(evaluator(move),move) for move in moves]
#end MaxPieces

def NoCorners(board, moves, color):
	"""
	Assesses the best strategy to win the game by avoiding high risk areas and taking corners
	Return format: [(value,(x_pos,y_pos)),(value,(x_pos,y_pos)),...]
	"""
	risk_val = -5
	max_risk = set([(1,1),(1,6),(6,1),(6,6)])
	mid_risk = set([(1,2),(1,3),(1,4),(1,5),(6,2),(6,3),(6,4),(6,5),(2,1),(3,1),(4,1),(5,1),(2,6),(3,6),(4,6),(5,6)])
	
	reward_val = -risk_val
	max_reward = set([(0,0),(0,7),(7,0),(7,7)])
	mid_reward = set([(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0), (0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 7), (1, 7), (2, 7), (3, 7), (4, 7), (5, 7), (6, 7), (7, 7), (7, 0), (7, 1), (7, 2), (7, 3), (7, 4), (7, 5), (7, 6), (7, 7)])
	
	retVal = []
	for a_move in moves:
		value = 0
		if a_move in max_risk:
			value-=2*risk_val
		elif a_move in mid_risk:
			value-=risk_val
		elif a_move in mid_reward:
			value+=reward_val
		elif a_move in max_reward:
			value+=2*reward_val
		retVal.append((value,a_move))
	
	return retVal
#end

def WinningHeuristic(board, moves, color):
	max_pieces = MaxPieces(board, move, color)
	best_strategy = NoCorners(board, move, color)
	retVal = []
	# merge the two lists into one
	# this changes the order of the tuple from (val,(x,y)) to ((x,y),val)
	partials1 = [x[::-1] for x in max_pieces]
	partials1.sort()
	partials2 = [x[::-1] for x in best_strategy]
	partials2.sort()

	for i in range(len(max_pieces)):
		piece = partials1[i]
		strat = partials2[i]
		sum_val = piece[1]+strat[1] # this is really all we needed.
		# Dev check
		assert piece[0] == strat[0] # Check during development. (x,y)==(x,y) or else this is all wrong.
		retVal.append((sum_val,piece[0]))

	return retVal
#end WinningHeuristic
Win=WinningHeuristic
