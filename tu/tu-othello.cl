
(defmacro while (test &body body)
	`(do ()
		((not ,test))
		,@body))


;;used to split string based on the separator string
(defun split-str (string &optional (separator " "))
  (split-str-1 string separator))

(defun split-str-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
		(split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
     	 (cons string r))))		

 
;;Representation of the board: an array of 100 slots ->
;;the legal positions are from 11 to 88 and excluding positions: 19 20 29 30 39 40 49 50 59 60 69 70 79 80
(defconstant move-directions '(-11 -9 9 -1 1 -10 10 11))
(defconstant empty-side '(19 20 29 30 39 40 49 50 59 60 69 70 79 80))

;;;type of plays in can be put into the board
(defconstant empty "_" "emtpy slot") 
(defconstant black "B" "black play")
(defconstant white "W" "white play")
(defconstant outer "X" "outer board")

(defconstant board-slots (loop for i from 11 to 88 when (<= 1 (mod i 10) 8) collect i) "all slots of the board users can play")


;;; define data types:
(deftype board-square () '(string))
(deftype ol-board () '(simple-array board-square (100)))

;;;;;;;;;;;; boar operators;;;;;;;;;;;;;;

(defun copy-board (board)    ;;;make a copy of the board
	(copy-seq board))

(defun opponent (type) (if (equalp type black) white black)) ;;; opponent of current player if black-> white, if white -> black

;;; list all the positions where a player can play
(defun legal-pos (type board)
  (loop for move in board-slots
	when (legal-move? move type board) collect move))

;;;make the intial board with 4 intial peices
(defun init-board ()
	(let ((board (make-array 100 :initial-element outer)))
		(dolist (square board-slots)
			(setf (aref board square) empty))
		(setf (aref board 44) white (aref board 45) black
			  (aref board 54) black (aref board 55) white)
		board))

;; display board in pretty format -> coupling with score of each player
(defun display-board (board)
	(loop for row from 1 to 8 do 
		(format t "~&" (* 10 row))
		(loop for col from 1 to 8
			for piece = (aref board (+ col (* 10 row)))
			do (format t "~a " piece)))
	(format t "~& B: ~d    W: ~d   ~d"
          (count-if #'(lambda (x) (equalp black x)) board)
          (count-if #'(lambda (x) (equalp white x)) board)
          (get-difference black board)))

;;;; a valid move is not at the edges of the board
(defun valid-move? (pos)
	(and (<= 11 pos 88) (not (member pos '(19 20 29 30 39 40 49 50 59 60 69 70 79 80)))))


;;; a legal moves will have adjcent opponent and will flip some plays
(defun legal-move? (pos type board)
	(and (equalp (aref board pos) empty)
		 (some #'(lambda (direction) (would-flip? pos type board direction))
			move-directions)))

;;; place the move to the board and flip plays in all directions if allowed
(defun make-move (pos type board)
	(setf (aref board pos) type)
	(dolist (direction move-directions)
		(flip-squares pos type board direction))
	board)

;;; flip based on the the type, and direction -> play in the direction with become the type until the bracketer is reached
(defun flip-squares (pos type board direction)
	(let ((bracketer (would-flip? pos type board direction)))
		(when bracketer
			(loop for c from (+ pos direction) by direction until (equalp c bracketer)
				do (setf (aref board c) type)))))

;;; check if a play will flip plays in a direction
(defun would-flip? (pos type board direction)
	(let ((next (+ pos direction)))
		(and (equalp (aref board next) (opponent type))
			 (find-me-following-direction (+ next direction) type board direction))))
				
;;; find the bracketer in a direction. 
(defun find-me-following-direction (pos type board direction)
	(cond ((equalp (aref board pos) type) pos)
	      ((equalp (aref board pos) (opponent type))
			(find-me-following-direction (+ pos direction) type board direction))
		  (t nil)))

;;; find if there is any legal moves
(defun any-legal-move? (type board)
	(some #'(lambda (pos) (legal-move? pos type board))
        board-slots))
;;;;; greedy strategy -> always aims for maximizing the number of slots		


;;; find different of score of a player with its opponent
(defun get-difference (type board)
	(- (count-if #'(lambda (x) (equalp type x)) board)
	   (let ((op (opponent type)))
	   	(count-if #'(lambda (x) (equalp op x)) board))))



;;; determing the wining and losing scores 
(defconstant winning-value most-positive-fixnum)
(defconstant losing-value  most-negative-fixnum)
(defun final-value (type board)
	(case (signum (get-difference type board))
		(-1 losing-value)
		( 0 0)
		(+1 winning-value)))


;;;;;; heuristic 2;;;;;;;;;;;;;;;;;;;;

;;;;; heuristic: each slot has a predetermined score
(defparameter *predetermied-scores*
  '#(0   0   0  0  0  0  0   0   0 0
     0 120 -20 20  5  5 20 -20 120 0
     0 -20 -40 -5 -5 -5 -5 -40 -20 0
     0  20  -5 15  3  3 15  -5  20 0
     0   5  -5  3  3  3  3  -5   5 0
     0   5  -5  3  3  3  3  -5   5 0
     0  20  -5 15  3  3 15  -5  20 0
     0 -20 -40 -5 -5 -5 -5 -40 -20 0
     0 120 -20 20  5  5 20 -20 120 0
     0   0   0  0  0  0  0   0   0 0))

;;; heuristic function: get sum of all the predetermined scores where player has its play
(defun get-sum-score (type board)
	(let ((opp (opponent type))
		  (score 0))
		(dolist (slot board-slots)
			(cond ((equalp (aref board slot) type)
					(setf score (+ score (aref *predetermied-scores* slot))))
				  ((equalp (aref board slot) opp)
				  	(setf score (- score (aref *predetermied-scores* slot))))))
		(+ score (get-difference type board))))


;;; get neighbors of the corner 
(defun corner-adjacents (corner)
	(case corner
		(11 '(12 21 22))
		(18 '(17 27 28))
		(81 '(82 72 71))
		(88 '(78 77 87))))

;;; this agent does some analyzing to the board:
;;; if player takes corner already at any adjacent positions to the corner, if one of them is ocuppied
;;; then it is more incentive for the player to takes play next to those adjacents 
(defun predetermined-score-sum-agent (type board)
	(let ((score (get-sum-score type board)))
		(dolist (corner '(11 18 81 88))
			(when (not (equalp (aref board corner) empty))
				(dolist (ad (corner-adjacents corner))
					(when (not (equalp (aref board ad) empty))
						(incf score (* (- 5 (aref *predetermied-scores* ad))
										  (if (equalp (aref board ad) type)
										  	+1 -1)))))))
		score))


;;; alpha beta function -->calling alpha-beta with the heuristic function to get the best move, return Null if there is no move
(defun alpha-beta (depth eval-fn)
	#'(lambda (type board)
		(multiple-value-bind (value move)
			(alpha-beta-helper type board losing-value winning-value
					depth eval-fn) 
		move)))

;;;; helper to alpha-beta : does actual job of exploring the game:
(defun alpha-beta-helper (type board max min depth eval-func) ;;; max is the reachable score, and min is the higher limit to prune the branches
	(if (eql depth 0) ;;reach depth -> evaluate the score
		(funcall eval-func type board)
		(let ((possible-positions (legal-pos type board))) 
			(if (null possible-positions) ;; if there no moves can be made -> find if opponent can move and continue explore
				(if (any-legal-move? (opponent type) board)
					(- (alpha-beta-helper (opponent type) board (- min) (- max) (1- depth) eval-func)) ;;; <--- this is opponent moves
					(final-value type board)) ;;; this case is reached when the player dont have any move as well as its opponent -> determine if its a wining or a lost
				(let ((chosen-pos (first possible-positions))) ;;if there is move 
					(dolist (pos possible-positions) ;;; for all the legal moves:
						(if (>= max min) ;;; if score achieved is greater than the limit -> prunt it by returing 
							(return)
							(let ((board-after-play (make-move pos type (copy-board board)))) ;; otherwise exploring the game with depths
								(let ((val (- (alpha-beta-helper (opponent type) board-after-play (- min) (- max) (1- depth) eval-func)))) ;; start with opponent first
									(when (> val max) ;; at the branch level -> if get better results -> updating the max to new value
										(setf chosen-pos pos max val))))))
					(values max chosen-pos))))))



;;;;;;; defining the agents ;;;;;;;;;;;;;;;;;;

;;; random agent;;;;;;;;;;
(defun random-agent (type board)
	(let ((positions (legal-pos type board)))
		(let ((el (random (length positions))))
			(print (nth (random (length positions)) positions)))))


;;; greedy agent finds the place where it maximizes its score
(defun greedy-agent (type board)
	(let* ((positions (legal-pos type board))
		   (scores (mapcar #'(lambda (pos) 
								(get-difference 
									type 
									(make-move pos type (copy-board board))))
							positions))
			(best (apply #'max scores)))
		(elt positions (position best scores))))


(defun alpha-beta-greedy-agent (depth)
	(alpha-beta depth #'get-difference))

(defun alpha-beta-predetermined-score (depth)
	(alpha-beta depth #'predetermined-score-sum-agent ))

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
				(setf board (make-move next-move current-player board))
				(setf current-player (next-to-play board current-player))
				(display-board board)))
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
	(let* ((strat (alpha-beta-predetermined-score depth))
		  (my-pos (funcall strat type input)))
		(if my-pos 
			(multiple-value-bind (col row) (floor my-pos 10)
				(princ (format nil "~d ~d" (1- col) (1- row))))
			(princ "-1 -1"))))


(defvar a)
(defun storing ()
	(setf a (read-line)))


(if (eql (length *args*) 0)
	(let ((strat1 (alpha-beta-predetermined-score 5))
		 (strat2 (alpha-beta-greedy-agent 5)))
		(play-game strat1 strat2))
	(progn 
		(storing)
		(othello-play (car *args*) (parse-integer (cadr *args*)) (translate-board a))))


