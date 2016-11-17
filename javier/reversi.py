# COMP472: Artificial Intelligence
# Javier E. Fajardo - 26487602
# reversi.py: everything related to the rules of the game and the board

black = "B"
white = "W"
empty = "0"

def parse_board(in_string):
	"""
	Parse a given board in the expected format for this assignment
	'((00000000)(00000000)(00000000)(000WB000)(000BW000)(00000000)(00000000)(00000000))'
	
	Returns a 2D array representing the board
	"""
	if(len(in_string)<64):
		raise ValueError("Provided string is too short for a Reversi board")
	
	array_format = in_string[2:-2].split(')(')
	return [list(x) for x in array_format]
#end board_parse

def valid_moves(board_state, color_symbol):
	"""
	Get all possible moves that can be made from the current board_state by a color_symbol

	Returns a list of tuples as (x,y) for all valid moves in the board
	"""
	
	pass
#end valid_moves

def is_game_over(board_state):
	"""
	Check if the game cannot continue
	
	Returns True for game over state
	"""
	pass
#end is_game_over

def check_winner(board_state):
	"""
	Return the color_symbol of whomever won the game
	"""
	pass
#end check_winner