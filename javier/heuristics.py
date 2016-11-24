# COMP472: Artificial Intelligence
# Javier E. Fajardo - 26487602
# heuristics.py: defines all heuristics that can be used in weighing decisions in a game algorithm

def MaxPieces(board, move, color):
	#TODO: count the amount of pieces that would be flipped by playing 'move' as 'color' on 'board' 
	pass
#end

def NoCorners(board, move, color):
	#TODO: just don't take high risk areas in the 8x8 board (rows before last)
	pass
#end

def WinningHeuristic(board, move, color):
	return MaxPieces(board, move, color) + NoCorners(board, move, color)
#end WinningHeuristic
