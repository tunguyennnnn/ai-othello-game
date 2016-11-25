# COMP472: Artificial Intelligence
# Javier E. Fajardo - 26487602
# reversi.py: everything related to the rules of the game and the board

black = "B"
white = "W"
empty = "0"

def is_valid(x,y):
	"""Check if x,y are within bounds"""
	return x < 8 and x >= 0 and y < 8 and y>=0
#end is_valid
def is_valid_and_empty(board,x,y):
	"""Check if within bounds and with an empty symbol"""
	return is_valid(x,y) and board[x][y]==empty
#end is_valid_and_empty

# Generates a new list along a one dimension slice of the board
def slice_board(board,direction,x,y):
	"""
	Returns a one dimensional view of the board along direction starting at x,y
	"""
	return [board[x+(i*direction[0])][y+(i*direction[1])] for i in range(8) if is_valid(x+(i*direction[0]), y+(i*direction[1])) ]
#end slice_board

def get_sliced_coords(board,direction,x,y):
	"""
	Returns sliced coordinates
	"""
	return [(x+(i*direction[0]),y+(i*direction[1])) for i in range(8) if is_valid(x+(i*direction[0]), y+(i*direction[1])) ]
#end get_sliced_coords

def get_counterpart(symbol):
	"""
	Return the opposite to symbol
	"""
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

def print_board(board):
	"""
	Pretty-prints the board onto the terminal
	"""
	print " ".join(['_']*9)
	for row in board:
		print "|%s|"% (" ".join(row)).replace(empty,'-')
	# end for
	print " ".join(['T']*9)
#end print_board

def serialize_board(board):
	"""
	Transforms the board back to a string
	
	Returns the serialized version of the board that can be parsed by 'parse_board'
	"""
	cereal = []
	for row in board:
		cereal.append("".join(row))
	#end for
	cereal = "(("+")(".join(cereal)+"))"
	return cereal
#end serialize_board

def valid_moves(board_state, color_symbol):
	"""
	Get all possible moves that can be made from the current board_state by a color_symbol

	Returns a list of tuples as (x,y) for all valid moves in the board
	"""
	def slice_and_check(x,y,direction):
		#slice
		single = slice_board(board_state,direction,x,y)
		retVal = color_symbol in single
	
		#if retVal:
				#print(str(direction)+"->"+str(single))
	
		return retVal
	#end inner slice_and_check

	# Check the direction
	check_dir = lambda A,B : (A[0]-B[0],A[1]-B[1])
	
	# 1. Map out the symbols
	other_symbol = get_counterpart(color_symbol)
	own_symbols = []
	counter_symbols = []
	for i in range(8):
		row = board_state[i]
		row_pieces = [(x,i) for x in range(8) if row[x]==color_symbol]
		own_symbols.extend(row_pieces)
		row_pieces = [(x,i) for x in range(8) if row[x]==other_symbol]
		counter_symbols.extend(row_pieces)
	#end for
	
	# 2. Check which placements would be valid
	num_own_symbols = len(own_symbols)
	valid_moves = set()
	for position in counter_symbols:
		x = position[0]
		y = position[1]
		adjacents = ((x-1,y+1),(x-1,y),(x-1,y-1),(x,y+1),(x,y-1),(x+1,y+1),(x+1,y),(x+1,y-1))
		for item in adjacents:
			if is_valid_and_empty(board_state,item[0],item[1]):
				direction = check_dir(position,item)
				if(slice_and_check(item[0],item[1],direction)):
					valid_moves.add(item)
		#end for item
	#end for position

	return list(valid_moves)
#end valid_moves

def check_flip_num(board,future_move,color_symbol):
	"""
	Check how many pieces are flipped if color_symbol plays future_move in board
	
	Return flipped amount
	"""
	directions = ((-1,+1),(-1,0),(-1,-1),(0,+1),(0,-1),(+1,+1),(+1,0),(+1,-1))
	x = future_move[0]
	y = future_move[1]
	flipped = 0
	other_symbol = get_counterpart(color_symbol)
	
	for direction in directions:
		# slice the board in a single dimension
		single_dimension = slice_board(board,direction,x,y)
		single_dimension = single_dimension[1:]
		partials = 0
		# Sum up the partials
		for symbol in single_dimension:
			# Nothing to flip
			if symbol==empty:
				break
			if symbol==other_symbol:
				partials+=1
			if symbol==color_symbol:
				flipped+=partials
				break
		#end for symbol
	#end for direction
	
	return flipped
#end check flip num

def play_move(board,move,color_symbol):
	"""
	Plays move on board using color_symbol.
	
	Returns a modified version of the original board with the move having been played
	"""
	# Actually play the move
	next_board = board
	x = move[0]
	y = move[1]
	next_board[x][y] = color_symbol
	
	# Flip all the pieces necessary
	directions = ((-1,+1),(-1,0),(-1,-1),(0,+1),(0,-1),(+1,+1),(+1,0),(+1,-1))
	flipped = 0
	
	other_symbol = get_counterpart(color_symbol)
	for direction in directions:
		# slice the board in a single dimension
		single_dimension = get_sliced_coords(board,direction,x,y)
		single_dimension = single_dimension[1:]

		stop_idx = 0
		# Check when to stop
		for i in range(len(single_dimension)):
			a_place=single_dimension[i]
			symbol = next_board[a_place[0]][a_place[1]]
			# Nothing to flip
			if symbol==empty or symbol==color_symbol:
				break
			if symbol==other_symbol:
				stop_idx+=1
		#end for
		
		flipped+=stop_idx
		if stop_idx>0:
			for i in range(stop_idx+1):
				move = single_dimension[i]
				next_board[move[0]][move[1]] = color_symbol
			#end for stop_idx
		#end if
	#end for directions
	
	# return the modified board
	return next_board
#end play_move

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
	num_black = 0
	num_white = 0
	for row in board_state:
		for column in row:
			if column==black:
				num_black+=1
			elif column==white:
				num_white+=1
	#end board_state fors

	if num_black < num_white:
		return white
	elif num_white < num_black:
		return black
		
	# Tie
	return None
#end check_winner
