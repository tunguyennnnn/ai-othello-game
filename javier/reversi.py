# COMP472: Artificial Intelligence
# Javier E. Fajardo - 26487602
# reversi.py: everything related to the rules of the game and the board

black = "B"
white = "W"
empty = "0"

def is_valid(x,y):
	return x < 8 and x >= 0 and y < 8 and y>=0
#end is_valid
def is_valid_and_empty(board,x,y):
	return is_valid(x,y) and board[y][x]==empty
#end is_valid_and_empty

def get_counterpart(symbol):
	if symbol == black:
		return white
	elif symbol == white:
		return black
	else:
		return empty
#end get_counterpart

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
	valid_moves = set()
	
	# Map out the symbols
	other_symbol = get_counterpart(color_symbol)
	print(other_symbol)
	own_symbols = []
	counter_symbols = []
	for i in range(8):
		row = board_state[i]
		row_pieces = [(x,i) for x in range(8) if row[x]==color_symbol]
		own_symbols.extend(row_pieces)
		row_pieces = [(x,i) for x in range(8) if row[x]==other_symbol]
		counter_symbols.extend(row_pieces)
	#end for
	
	# Check which placements would be valid
	num_own_symbols = len(own_symbols)
	print(counter_symbols)
	for position in counter_symbols:
		x = position[0]
		y = position[1]
		adjacents = ((x-1,y+1),(x-1,y),(x-1,y-1),(x,y+1),(x,y-1),(x+1,y+1),(x+1,y),(x+1,y-1))
		print(adjacents)
		for item in adjacents:
			#TODO: missing check to get final piece
			if is_valid_and_empty(board_state,item[0],item[1]):
				pass
				#check if there's a piece that can connect in a straight line
			#	tangent = (item[0]-position[0], item[1]-position[1]) # Not going to work
					
				
		
	
	return valid_moves
#end valid_moves

def is_game_over(board_state):
	"""
	Check if the game cannot continue
	
	Returns True for game over state
	"""
	string_board = "".join(["".join(x) for x in board_state])
	return (empty not in string_board) or (black not in string_board) or (white not in string_board) or len(valid_moves(board_state,black))<1 or len(valid_moves(board_state,white))<1
#end is_game_over

def check_winner(board_state):
	"""
	Return the color_symbol of whomever won the game
	"""
	pass
#end check_winner
