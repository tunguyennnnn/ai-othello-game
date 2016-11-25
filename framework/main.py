import sys, os, subprocess, shlex
import subprocess as sb

class OthelloPlay:
    def __init__(self):
        self.black = "B"
        self.white = "W"
        self.empty = "_"
        self.board = [self.empty for i in range(64)] #middle
        self.board[27] = self.board[36] = "B"
        self.board[28] = self.board[35] = "W"

    def print_board(self):
        for i in range(8):
            for j in range(8):
                print repr(self.board[i*8 + j]).rjust(1),
            print ''
        print "Current count: B: %d ---- W: %d" %(self.count_difference("b"), self.count_difference('w'))

    def count_difference(self, type="B"):
        return self.board.count(type.upper()) - self.board.count(self.opponent(type.upper()))

    def opponent(self, type="B"):
        if type == self.black:
            return self.white
        else:
            return self.black
            
    def to_lisp_board(self, type = "B", depth=3,  board = None):
        if not board:
            board = self.board
        string_board = "%s %d" %(type, depth)
        for item in board:
            string_board += ' %s' %item
        return string_board

    def play_game(self, black_command = [], white_command = [], depth = 3):
        process = sb.Popen(black_command, stdin = sb.PIPE, stdout = sb.PIPE, stderr = sb.PIPE, bufsize = 0)
        print process.communicate(self.to_lisp_board())
        return process#end class Board

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