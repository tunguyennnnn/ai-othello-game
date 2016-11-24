;;; utilities 
(defun mapa-b (fn a b &optional (step 1)) ;; (mapa-b 0 10 2) -> '(0 2 4 8 10)
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

  
(defmacro while (test &body body)
	`(do ()
		((not ,test))
		,@body))

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

(defparameter *weights*
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




;;Representation of the board: an array of 100 slots ->
;;the legal positions are from 11 to 88
(defconstant move-directions '(-11 -9 -9 -1 1 9 10 11))

(defconstant empty "_" "emtpy slot")
(defconstant black "B" "black play")
(defconstant white "W" "white play")
(defconstant outer "X" "outer board")

(defconstant board-slots (mapa-b #'(lambda (i) i) 11 88) "all slots of the board users can play")

(deftype board-square () '(string))

	
(deftype ol-board () '(simple-array board-square (100)))

(defun copy-board (board)
	(copy-seq board))

(defun opponent (type) (if (equalp type black) white black))

(defun legal-pos (type board)
  (loop for move in board-slots
	when (legal-move? move type board) collect move))
	
(defun init-board ()
	(let ((board (make-array 100 :initial-element outer)))
		(dolist (square board-slots)
			(setf (aref board square) empty))
		(setf (aref board 44) white (aref board 45) black
			  (aref board 54) black (aref board 55) white)
		board))
			  
(defun display-board (board)
	(loop for row from 1 to 8 do 
		(format t "~&" (* 10 row))
		(loop for col from 1 to 8
			for piece = (aref board (+ col (* 10 row)))
			do (format t "~a " piece))))
	   
(defun valid-move? (pos)
	(and (<= 11 pos 88))) 

(defun legal-move? (pos type board)
	(and (equalp (aref board pos) empty)
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
			(loop for c from (+ pos direction) by direction until (equalp c bracketer)
				do (setf (aref board c) type)))))
				
(defun would-flip? (pos type board direction)
	(let ((next (+ pos direction)))
		(and (equalp (aref board next) (opponent type))
			 (find-me-following-direction (+ next direction) type board direction))))
				

(defun find-me-following-direction (pos type board direction)
	(cond ((equalp (aref board pos) type) pos)
	      ((equalp (aref board pos) (opponent type))
			(find-me-following-direction (+ pos direction) type board direction))
		  (t nil)))
		  
(defun any-legal-move? (type board)
	(some #'(lambda (pos) (legal-move? pos type board))
        board-slots))


;;;;; greedy strategy -> always aims for maximizing the number of slots		

		 
(defun get-difference (type board)
	(print board)
	(print type)
	(print (count type board))
	(print (opponent type))
	(print (count (opponent type) board))
	(- (count-if #'(lambda (x) (equalp type x)) board)
	   (let ((op (opponent type)))
	   	(count-if #'(lambda (x) (equalp op x)) board))))

(defun greedy-strat (type board)
	(let* ((positions (legal-pos type board))
		   (scores (mapcar #'(lambda (pos) 
								(get-difference 
									type 
									(make-move pos type (copy-board board))))
							positions))
			(best (apply #'max scores)))
		(elt positions (position best scores))))
		
(defun final-value (type board)
  "Is this a win, loss, or draw for player?"
  (case (signum (get-difference type board))
    (-1 losing-value)
    ( 0 0)
    (+1 winning-value)))


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
									(print depth)
									(display-board board-after-play)
									(print val)
									(when (> val max)
										(setf chosen-pos pos max val))))))
					(values max chosen-pos))))))



(defconstant winning-value most-positive-fixnum)
(defconstant losing-value  most-negative-fixnum)

(defun alpha-beta (depth eval-fn)
  #'(lambda (type board)
      (multiple-value-bind (value move)
          (alpha-beta-helper type board losing-value winning-value
                      depth eval-fn) 
        move)))
		
(defun play-game (strat1 strat2)
	(let ((board (init-board))
		  (current-player black))
		(while current-player
			(display-board board)
			(let ((next-move (funcall
									strat1
									 current-player									
									 board 
									 )))
				(setf board (make-move next-move current-player board))
				(setf current-player (next-to-play board current-player))))
		))
								
	
(defun next-to-play (board previous-type)
	(let ((opp (opponent previous-type)))
		(cond ((any-legal-move? opp board) opp)
			  ((any-legal-move? previous-type board)
				previous-type)
			  (t nil))))


(defun translate-board (input-lst)
	(let ((board (make-array 100 :initial-element outer)))
		(do ((input input-lst (cdr input))
			  (index 0 (1+ index)))
			 ((null input) board)
			 (multiple-value-bind (column row) (floor index 8)
			 	(setf (aref board (+ (* (1+ column) 10) row 1)) (car input))))))

(defun othello-play (input)
	(let ((type (car input))
		  (depth (parse-integer (cadr input)))
		  (board (translate-board (cddr input))))
		(let* ((strat (alpha-beta 3 #'get-difference))
				(my-pos (funcall strat type board)))
			(setf (aref board my-pos) type)
			(display-board board))))


(othello-play (split-str (car *args*)))



