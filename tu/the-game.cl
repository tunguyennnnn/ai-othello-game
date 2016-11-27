
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


(defun alpha-beta-helper (type board max min depth eval-func)
	(if (eql depth 0) ;;reach depth -> evaluate the 
		(funcall eval-func type board)
		(let ((possible-positions (legal-pos type board)))
			(if (null possible-positions) ;; if there no moves can be mad -> find if opponent can move
				(if (any-legal-move? (opponent type) board)
					(- (alpha-beta-helper (opponent type) board (- min) (- max) (1- depth) eval-func))
					(final-value type board))
				(let ((chosen-pos (first possible-positions)))
					(dolist (pos possible-positions)
						(if (>= max min)
							(return)
							(let ((board-after-play (make-move pos type (copy-board board))))
								(let ((val (- (alpha-beta-helper (opponent type) board-after-play (- min) (- max) (1- depth) eval-func))))
									(when (> val max)
										(setf chosen-pos pos max val))))))
					(values max chosen-pos))))))


(defun alpha-beta (depth eval-fn)
  #'(lambda (type board)
      (multiple-value-bind (value move)
          (alpha-beta-helper type board losing-value winning-value
                      depth eval-fn) 
        move)))
