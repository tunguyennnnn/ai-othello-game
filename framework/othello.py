# COMP472: Artificial Intelligence
# Tu Nguyen and Javier E. Fajardo

class OthelloPlay:
    def __init__(self):
        self.black = "B"
        self.white = "W"
        self.empty = "_"
        self.directions = ((-1,+1),(-1,0),(-1,-1),(0,+1),(0,-1),(+1,+1),(+1,0),(+1,-1))
        #self.board = [self.empty for i in range(64)] #middle
        #hard coded start state
        self.board = [['_', '_', '_', '_', '_', '_', '_', '_'], ['_', '_', '_', '_', '_', '_', '_', '_'], ['_', '_', '_', '_', '_', '_', '_', '_'], ['_', '_', '_', 'W', 'B', '_', '_', '_'], ['_', '_', '_', 'B', 'W', '_', '_', '_'], ['_', '_', '_', '_', '_', '_', '_', '_'], ['_', '_', '_', '_', '_', '_', '_', '_'], ['_', '_', '_', '_', '_', '_', '_', '_']]
        #self.board[27] = self.board[36] = "B"
        #self.board[28] = self.board[35] = "W"

    def add_to_board(self, type, row, col):
    	#should have check correct move
        if self.is_valid_move(type, row, col):
            self.board[row][col] = type
            self.flip_moves(type, row, col)
            return True
        else:
            return False
    def is_legal_move(self, row, col):
        return row >= 0 and row <= 7 and col <= 7  and col >= 0

    def is_valid_move(self, type, row, col):
        if self.board[row][col] == self.empty:
            for (row_dir, col_dir) in self.directions:
                print row_dir, col_dir, self.is_legal_move(row + row_dir, col + col_dir), self.find_bracket(type, row + row_dir + row_dir, col + col_dir + col_dir, row_dir, col_dir)
                if self.is_legal_move(row + row_dir, col + col_dir) and self.board[row + row_dir][col + col_dir] == self.opponent(type) and self.find_bracket(type, row + row_dir + row_dir, col + col_dir + col_dir, row_dir, col_dir):
                    return True
        return False

    def flip_moves(self, type, row, column):
    	for (row_adder, column_adder) in self.directions:
    		other_end = self.find_bracket(type, row + row_adder, column + column_adder, row_adder, column_adder)
    		if other_end:
    			(row_end, col_end) = other_end
    			copy_row, copy_col = row, column
    			while not (copy_row == row_end and copy_col == col_end):
    				copy_row += row_adder
    				copy_col += column_adder
    				self.board[copy_row][copy_col] = type

    def find_bracket(self,type, row, col, row_dir, col_dir):
    	if (not self.is_legal_move(row, col)) or self.board[row][col] == self.empty:
    		return None
    	else:
    		if type == self.board[row][col]:
    			return (row, col)
    		else:
    			return self.find_bracket(type, row + row_dir, col + col_dir, row_dir, col_dir)


    def print_board(self):
        for i in range(8):
            for j in range(8):
                print repr(self.board[i*8 + j]).rjust(1),
            print ''
        print "Current count: B: %d ---- W: %d" %(self.count_difference("b"), self.count_difference('w'))

    def serialize(self):
	cereal = []
	for row in self.board:
		cereal.append("".join(row).replace(self.empty,'0'))
	#end for
	cereal = "(("+")(".join(cereal)+"))"
	return cereal

    def printout(self):
	"""
	Pretty-prints the board onto the terminal
	"""
	print " ".join(['_']*9) + " X"
	for row in range(8):
		print "|%s| %d"% ((" ".join(self.board[row])).replace(self.empty,'-'),row)
	# end for
	print "Y"+" ".join([str(i) for i in range(8)])

    def count_difference(self, type="B"):
        return self.board.count(type.upper()) - self.board.count(self.opponent(type.upper()))

    def opponent(self, symbol="B"):
        if symbol == self.black:
            return self.white
        else:
            return self.black


#end OthelloPlay
