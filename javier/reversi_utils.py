# COMP472: Artificial Intelligence
# Javier E. Fajardo - 26487602
# reversi_utils.py: additional functions to evaluate the state of the game

from reversi import black,white,empty

def get_counterpart(symbol):
	if symbol is black:
		return white
	elif symbol is white:
		return black
	else:
		return empty
#end get_counterpart

def check_flip_num(board,future_move,color_symbol):
	"""
	Check how many pieces are flipped if color_symbol plays future_move in board
	"""
	def check_horizontal():
		pass
	#end
	def check_vertical():
		pass
	#end
	def check_diagonals():
		pass
	#end
	
	flipped = 0
	#flipped+=check_horizontal()
	#flipped+=check_vertical()
	#flipped+=check_diagonals()

	pass
#end check flip num