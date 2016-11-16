black = "B"
white = "W"
tu_outer_board = "X"
empty_slot = "_"

class Board:
    def __init__(self):
        self.black = "B"
        self.white = "W"
        self.empty = "_"
        self.board = [self.empty for i in range(64)]
        self.board[27] = self.board[35] = "B"
        self.board[28] = self.board[36] = "W"

    def print_board(self):
        for i in range(8):
            for j in range(8):
                print repr(self.board[i*8 + j]).rjust(1),
            print ''

    def count_difference(self, type="B"):
        return self.board.count(type.upper()) - self.board.count(self.opponent(type.upper()))

    def opponent(self, type="B"):
        if type == self.black:
            return self.white
        else:
            return self.black
            
    def to_lisp_board(self):
        pass
