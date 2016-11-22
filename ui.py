import Tkinter as tk
from threading import Thread
from multiprocessing import *
class GameBoard(tk.Frame, Thread):
    def __init__(self, parent, rows=8, columns=8, size=32):
        '''size is the size of a square, in pixels'''
        self.rows = rows
        self.columns = columns
        self.size = size
        self.blackImage = tk.PhotoImage(file="black.gif")
        self.whiteImage = tk.PhotoImage(file="white.gif")
        self.pieces = {}
        canvas_width = columns * size + 30
        canvas_height = rows * size + 100
        tk.Frame.__init__(self, parent)
        self.canvas = tk.Canvas(self, borderwidth=0, highlightthickness=0,
                                width=canvas_width, height=canvas_height, background="bisque")
        self.canvas.pack(side="top", fill="both", expand=True, padx=2, pady=2)

        # this binding will cause a refresh if the user interactively
        # changes the window size
        self.canvas.bind("<Configure>", self.refresh)
        self.canvas.bind("<Button-1>", self.clickHandler)

    def clickHandler(self, event):
        x   , y = event.x, event.y
        row = col = 0
        for r in range(self.rows):
            if (r + 1)*self.size >= x:
                row = r
                break
        for c in range(self.columns):
            if (c+1)*self.size  >= y:
                col = c
                break
        print (row, col)
        self.addBlack(col, row)
        return (row, col)


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
        self.canvas.create_text(x, y, width=self.rows*self.size, anchor="nw",text="Current player: %s Black's score = %d  White's score = %d" %(type, score_black, score_white))

    def addpiece(self, name, image, row=0, column=0):
        '''Add a piece to the playing board'''
        self.canvas.create_image(0, 0, image=image, tags=(name, "piece"), anchor="c")
        self.placepiece(name, row, column)

    def placepiece(self, name, row, column):
        '''Place a piece at the given row/column'''
        self.pieces[name] = (row, column)
        x0 = (column * self.size) + int(self.size/2)
        y0 = (row * self.size) + int(self.size/2)
        print x0, y0
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
        print self.pieces
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



# image comes from the silk icon set which is under a Creative Commons
# license. For more information see http://www.famfamfam.com/lab/icons/silk/
imagedata = '''
    R0lGODlhEAAQAOeSAKx7Fqx8F61/G62CILCJKriIHM+HALKNMNCIANKKANOMALuRK7WOVLWPV9eR
    ANiSANuXAN2ZAN6aAN+bAOCcAOKeANCjKOShANKnK+imAOyrAN6qSNaxPfCwAOKyJOKyJvKyANW0
    R/S1APW2APW3APa4APe5APm7APm8APq8AO28Ke29LO2/LO2/L+7BM+7BNO6+Re7CMu7BOe7DNPHA
    P+/FOO/FO+jGS+/FQO/GO/DHPOjBdfDIPPDJQPDISPDKQPDKRPDIUPHLQ/HLRerMV/HMR/LNSOvH
    fvLOS/rNP/LPTvLOVe/LdfPRUfPRU/PSU/LPaPPTVPPUVfTUVvLPe/LScPTWWfTXW/TXXPTXX/XY
    Xu/SkvXZYPfVdfXaY/TYcfXaZPXaZvbWfvTYe/XbbvHWl/bdaPbeavvadffea/bebvffbfbdfPvb
    e/fgb/Pam/fgcvfgePTbnfbcl/bfivfjdvfjePbemfjelPXeoPjkePbfmvffnvbfofjlgffjkvfh
    nvjio/nnhvfjovjmlvzlmvrmpvrrmfzpp/zqq/vqr/zssvvvp/vvqfvvuPvvuvvwvfzzwP//////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////yH+FUNyZWF0ZWQgd2l0aCBU
    aGUgR0lNUAAh+QQBCgD/ACwAAAAAEAAQAAAIzAD/CRxIsKDBfydMlBhxcGAKNIkgPTLUpcPBJIUa
    +VEThswfPDQKokB0yE4aMFiiOPnCJ8PAE20Y6VnTQMsUBkWAjKFyQaCJRYLcmOFipYmRHzV89Kkg
    kESkOme8XHmCREiOGC/2TBAowhGcAyGkKBnCwwKAFnciCAShKA4RAhyK9MAQwIMMOQ8EdhBDKMuN
    BQMEFPigAsoRBQM1BGLjRIiOGSxWBCmToCCMOXSW2HCBo8qWDQcvMMkzCNCbHQga/qMgAYIDBQZU
    yxYYEAA7
'''

class RunBoard(Process):
    def __init__(self):
        super(RunBoard, self).__init__()




    def run(self):
        root = tk.Tk()
        board = GameBoard(root)
        board.pack(side="top", fill="both", expand="true", padx=4, pady=4)
        player1 = tk.PhotoImage(file="black.gif")
        board.addBlack(3,4)
        board.addBlack(4,3)
        board.addWhite(3,3)
        board.addWhite(4,4)
        board.currentStatus("black", 0, 0)
        print "Xxxxxx"
        root.mainloop()

if __name__ == "__main__":
    x = RunBoard()
    x.start()
    x.join()
