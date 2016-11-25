# COMP472: Artificial Intelligence
# Tu Nguyen and Javier E. Fajardo

class OthelloPlay:
    def __init__(self):
        self.black = "B"
        self.white = "W"
        self.empty = "_"
        #self.board = [self.empty for i in range(64)] #middle
        #hard coded start state
        self.board = [['_', '_', '_', '_', '_', '_', '_', '_'], ['_', '_', '_', '_', '_', '_', '_', '_'], ['_', '_', '_', '_', '_', '_', '_', '_'], ['_', '_', '_', 'W', 'B', '_', '_', '_'], ['_', '_', '_', 'B', 'W', '_', '_', '_'], ['_', '_', '_', '_', '_', '_', '_', '_'], ['_', '_', '_', '_', '_', '_', '_', '_'], ['_', '_', '_', '_', '_', '_', '_', '_']]
        #self.board[27] = self.board[36] = "B"
        #self.board[28] = self.board[35] = "W"

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

    def count_difference(self, type="B"):
        return self.board.count(type.upper()) - self.board.count(self.opponent(type.upper()))

    def opponent(self, symbol="B"):
        if symbol == self.black:
            return self.white
        else:
            return self.black
            
    def to_lisp_board(self, symbol = "B", depth=3,  board = None):
        if not board:
            board = self.board
        string_board = "%s %d" %(symbol, depth)
        for item in board:
            string_board += ' %s' %item
        return string_board

    def play_game(self, black_command = [], white_command = [], depth = 3):
        process = sb.Popen(black_command, stdin = sb.PIPE, stdout = sb.PIPE, stderr = sb.PIPE, bufsize = 0)
        print process.communicate(self.to_lisp_board())
        return process#end class Board
#end OthelloPlay