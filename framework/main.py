import sys, os, subprocess, shlex


# TODO: remove Globals :)
black = "B"
white = "W"
tu_outer_board = "X"
empty_slot = "_"

# TODO: move to separate file
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
#end class Board

def main(args):
	#run the command, use manual buffering into a file
	testOut = open("test.log",'w')
	testRead = open("test.log",'r')
	for arg in args:
		cmd = shlex.split(arg)
		print "spawning %s" % cmd
		p = subprocess.Popen(cmd, stdin=subprocess.PIPE,stdout=testOut,stderr=subprocess.PIPE)
		print testRead.readline()  # Prints empty line
		#kill the process immediately
		while(p.poll() is None):
			p.kill()
		print "process killed (%s) " % p.poll()
	#end for
	print "closing"
		
	#end for
	testOut.close()
	testRead.close()
#end main

#entry point
if __name__=="__main__":
	print "Othello engine starting"
	main(sys.argv[1:])