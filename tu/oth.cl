;;; utilities 
(defun mapa-b (fn a b &optional (step 1)) ;; (mapa-b 0 10 2) -> '(0 2 4 8 10)
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

;;Representation of the board: an array of 100 slots ->
;;the legal positions are from 11 to 88
(defconstant move-directions '(-11 -9 -9 -1 1 9 10 11))

(defconstant empty '_ "emtpy slot")
(defconstant black 'B "black play")
(defconstant white 'W "white play")
(defconstant outer 'X "outer board")

(defconstant board-slots (mapa-b #'(lambda (i) i) 11 88) "all slots of the board users can play")

(deftype board-square () '(symbol))

	
(deftype ol-board () '(simple-array board-square (100)))

(defun copy-board (board)
	(copy-seq board))

(defun opponent (player) (if (eql player black) white black))

(defun legal-moves (type board)
  (loop for move in board-slots
	when (legal-p move type board) collect move))
	
(defun init-board ()
	(let ((board (make-array 100 :initial-element outer)))
		(dolist (square board-slots)
			(setf (aref board square) empty))
		(setf (aref board 44) white (aref board 45) black
			  (aref board 54) black (aref board 55) white)
		board))
			  
(defun display-board (board)
	(loop for row from 1 to 8 do 
		(format t "~& ''d " (* 10 row))
		(loop for col from 1 to 8
			for piece = (aref board (+ col (* 10 row)))
			do (format t "~a " piece))))
			
			
(defun current-difference (board type)
	(- (count type board)
	   (count (opponent type) board)))
	   
(defun valid-move? (pos)
	(and (<= 11 pos 88))) 

(defun legal-move? (pos type board)
	(and (eql (aref board pos) empty)
		 (some #'(lambda (direction) (would-flip? pos type board direction))
			move-directions)))
			
(defun make-move (pos type board)
	(setf (aref board pos) type)
	(dolist (direction move-directions)
		(flip-squares pos type board direction))
	board)

(defun flip-squares (pos type board direction)
	(let ((bracketer (would-flip? pos type board direction)))
		(when bracketer
			(loop for c from (+ pos direction) by direction until (eql c bracketer)
				do (setf (aref board c) type)))))
				
(defun would-flip? (pos type board direction)
	(let ((next (+ pos direction)))
		(and (eql (aref board next) (opponent type))
			 (find-me-following-direction (+ next direction) type board direction))))
				

(defun find-me-following-direction (pos type board direction)
	(cond ((eql (aref board pos) type) pos)
	      ((eql (aref board pos) (opponent type))
			(find-me-following-direction (+ pos direction) type board direction))
		  (t nil)))
		  
(defun any-legal-move? (type board)
	(some #'(lambda (pos) (legal-move? pos type board))
        board-slots))
		  
		  
;;;;; minimax
(defun alpha-beta (player board achievable cutoff ply eval-fn)
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (legal-moves player board)))
        (if (null moves)
            (if (any-legal-move? (opponent player) board)
                (- (alpha-beta (opponent player) board
                               (- cutoff) (- achievable)
                               (- ply 1) eval-fn))
                (final-value player board))
            (let ((best-move (first moves)))
              (loop for move in moves do
                (let* ((board2 (make-move move player
                                          (copy-board board)))
                       (val (- (alpha-beta
                                 (opponent player) board2
                                 (- cutoff) (- achievable)
                                 (- ply 1) eval-fn))))
                  (when (> val achievable)
                    (setf achievable val)
                    (setf best-move move)))
                until (>= achievable cutoff))
              (values achievable best-move))))))

(defun alpha-beta-searcher (depth eval-fn)
  "A strategy that searches to DEPTH and then uses EVAL-FN."
  #'(lambda (player board)
      (multiple-value-bind (value move)
          (alpha-beta player board losing-value winning-value
                      depth eval-fn) 
        (declare (ignore value))
        move)))
		

			