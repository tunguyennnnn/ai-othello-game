import Tkinter as tk
import os
from time import sleep
from othello import OthelloPlay
from player import *

class GameBoard(tk.Frame):
    def __init__(self, parent, cpu_player = None, rows=8, columns=8, size=64):
        '''size is the size of a square, in pixels'''
        self.rows = rows
        self.root = parent
        self.columns = columns
        self.size = size
        self.blackImage = tk.PhotoImage(file=os.path.join(os.getcwd(),"black.gif"))
        self.whiteImage = tk.PhotoImage(file=os.path.join(os.getcwd(),"white.gif"))
        self.pieces = {}
        self.human_play = True
        canvas_width = columns * size + 30
        canvas_height = rows * size + 100
        tk.Frame.__init__(self, parent)
        self.canvas = tk.Canvas(self, borderwidth=0, highlightthickness=0,
                                width=canvas_width, height=canvas_height, background="bisque")
        self.canvas.pack(side="top", fill="both", expand=True, padx=2, pady=2)

        # this binding will cause a refresh if the user interactively
        # changes the window size
        self.canvas.bind("<Configure>", self.refresh)
        if cpu_player:
            self.cpu_player = cpu_player
            self.gameboard = OthelloPlay()
            self.update_board(self.gameboard.board)
            self.wrong_counter = 0
            self.canvas.bind("<Button-1>", self.clickHandler)

    def clickHandler(self, event):
        if self.human_play:
            x, y = event.x, event.y
            row = col = 0
            for r in range(self.rows):
                if (r + 1)*self.size >= x:
                    row = r
                    break
            for c in range(self.columns):
                if (c+1)*self.size  >= y:
                    col = c
                    break
            if self.gameboard.add_to_board(self.gameboard.black, row, col):
                self.update_board(self.gameboard.board)
                self.gameboard.printout()
                self.human_play = False
                cpu_move = self.cpu_player.play(self.gameboard.serialize())
                print cpu_move
                if not cpu_move == (-1,-1):
                    (row, col) = cpu_move
                    self.gameboard.add_to_board(self.gameboard.white, row, col)
                    self.update_board(self.gameboard.board)
                self.gameboard.printout()
                self.human_play = True
                self.wrong_counter = 0
            else:
                if self.wrong_counter < 2:
                    self.wrong_counter += 1
                    self.currentStatus("You have played an illegal moved, please play gain")
                else:
                    cpu_move = self.cpu_player.play(self.gameboard.serialize())
                    print cpu_move
                    if not cpu_move == (-1,-1):
                        (row, col) = cpu_move
                        self.gameboard.add_to_board(self.gameboard.white, row, col)
                        self.update_board(self.gameboard.board)
                    self.gameboard.printout()
                    self.human_play = True
                    self.wrong_counter = 0


    def update_board(self, play_board):
        black_count = white_count = 0;
    	self.canvas.delete("piece")
    	for (row, sublist) in zip(range(8), play_board):
    		for (col, player_type) in zip(range(8), sublist):
    			if player_type == "B":
    				black_count += 1
    				self.addBlack(col, row)
    			elif player_type == "W":
    				white_count += 1
    				self.addWhite(col, row)
    	self.currentStatus("B", black_count, white_count)
    	self.root.update()


    def addBlack(self, row=0, column = 0):
        self.addpiece("%d %d" %(row, column), self.blackImage, row, column)

    def addWhite(self, row=0, column = 0):
        self.addpiece("%d %d" %(row, column), self.whiteImage, row, column)

    def flipPiece(self, row=0, column=0):
        if 'pyimage1' in self.canvas.itemconfig("%d %d" %(row, column))["image"]:
            self.canvas.itemconfig("%d %d" %(row, column), image=self.whiteImage)
        elif 'pyimage2' in self.canvas.itemconfig("%d %d" %(row, column))["image"]:
            self.canvas.itemconfig("%d %d" %(row, column), image=self.blackImage)

    def currentStatus(self, type="Black", score_black=0, score_white=0):
        x = 30
        y = self.columns * self.size + self.size
        self.canvas.itemconfig("status",text="Current player: %s Black's score = %d  White's score = %d" %(type, score_black, score_white))

    def addpiece(self, name, image, row=0, column=0):
        '''Add a piece to the playing board'''
        self.canvas.create_image(0, 0, image=image, tags=(name, "piece"), anchor="c")
        self.placepiece(name, row, column)

    def placepiece(self, name, row, column):
        '''Place a piece at the given row/column'''
        self.pieces[name] = (row, column)
        x0 = (column * self.size) + int(self.size/2)
        y0 = (row * self.size) + int(self.size/2)
        self.canvas.coords(name, x0, y0)



    def refresh(self, event):
        '''Redraw the board, possibly in response to window being resized'''
        xsize = int((event.width-1) / self.columns)
        ysize = int((event.height-1) / self.rows)
        self.size = min(xsize, ysize)
        for row in range(self.rows):
            for col in range(self.columns):
                x1 = (col * self.size)
                y1 = (row * self.size)
                x2 = x1 + self.size
                y2 = y1 + self.size
                self.canvas.create_rectangle(x1, y1, x2, y2, outline="black", fill="gray", tags="square")
                if row == self.rows-1 and col == self.columns-1:
                    self.canvas.create_rectangle(0, y2, x2, y2 + 30, outline="black", fill="white", tags="square")
        x = 30
        y = self.columns * self.size
        self.canvas.create_text(x, y, tag="status", width=self.rows*self.size, anchor="nw",text="Current player: %s Black's score = %d  White's score = %d" %("B", 0, 0))
        for name in self.pieces:
            self.placepiece(name, self.pieces[name][0], self.pieces[name][1])
        self.canvas.tag_raise("piece")
        self.canvas.tag_lower("square")

    def run(self):
        root = tk.TK()
        self.addBlack(3,4)
        self.addBlack(4,3)
        self.addWhite(3,3)
        self.addWhite(4,4)
        root.mainloop()
