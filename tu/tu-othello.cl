(require :the-game)

;; for testing the agents -> s
(defun play-game (agent1 agent2)	
	(let ((board (init-board))
		  (current-player black))
		(while current-player
			(let ((next-move (funcall
									(if (equalp current-player black)
										agent1
										agent2)
									 current-player									
									 board 
									 )))
				(print next-move)
				(setf board (make-move next-move current-player board))
				(setf current-player (next-to-play board current-player))
				(display-board board)
				(print (equalp current-player black))))
		))
								

;;determined which one can play next -> cover case when a player cannot make a move
(defun next-to-play (board previous-type)
	(let ((opp (opponent previous-type)))
		(cond ((any-legal-move? opp board) opp)
			  ((any-legal-move? previous-type board)
				previous-type)
			  (t nil))))


(defun translate-board (str)
	(let ((board (make-array 100 :initial-element outer))
		  (input '()))
		(do ((index 0 (1+ index))
			 (board-index 11))
			((or (>= index (length str)) (> board-index 88)) (return))
			(progn 
				(while (member board-index empty-side)
					(incf board-index))
				(cond ((equalp (aref str index) #\B)
						(progn 
							(setf (aref board board-index) black)
							(incf board-index)))
					  ((equalp (aref str index) #\W)
						(progn 
							(setf (aref board board-index) white)
							(incf board-index)))
					  ((equalp (aref str index) #\0)
						(progn 
							(setf (aref board board-index) empty)
							(incf board-index))))))
		board))


(defun othello-play (type depth input)
	(let* ((strat (alpha-beta 3 #'predetermined-score-sum-agent))
		  (my-pos (funcall strat type input)))
		(if my-pos 
			(multiple-value-bind (col row) (floor my-pos 10)
				(princ (format nil "~d ~d" (1- col) (1- row))))
			(princ "-1 -1"))))


(defvar a)
(defun storing ()
	(setf a (read-line)))


(if (eql (length *args*) 0)
	(let ((strat1 (alpha-beta 3 #'get-difference))
		 (strat2 (alpha-beta 3 #'predetermined-score-sum-agent)))
		(play-game strat1 strat2))
	(progn 
		(storing)
		(othello-play (car *args*) (cadr *args*) (translate-board a))))


