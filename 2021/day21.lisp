(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)
(ql:quickload :priority-queue)
(ql:quickload :cl-ppcre)
(ql:quickload :trivial-escapes)
(ql:quickload :fare-memoization)

(named-readtables:in-readtable trivial-escapes:readtable)

(defun parse-input (input)
  (destructuring-bind (player1 player2) (str:lines input)
    (let ((player1 (cadr (str:split ": " player1)))
	  (player2 (cadr (str:split ": " player2))))
      (cons (parse-integer player1) (parse-integer player2)))))

(defun aoc-2021-day21-part1 (input)
  (let ((player1-spot (car input))
	(player2-spot (cdr input))
	(player1-score 0)
	(player2-score 0)
	(die 1)
	(die-thrown 0)
	(current-player 1))
    (loop until (or (>= player1-score 1000)
		    (>= player2-score 1000))
	  if (and (< player2-score 1000) (= current-player 1))
	    do
	       (let ((steps 0))
		 (incf steps die)

		 (progn
		   (incf die)
		   (when (> die 100)
		     (decf die 100)))

		 (incf steps die)

		 (progn
		   (incf die)
		   (when (> die 100)
		     (decf die 100)))

		 (incf steps die)

		 (progn
		   (incf die)
		   (when (> die 100)
		     (decf die 100)))

		 (incf die-thrown 3)

		 (incf player1-spot steps)
		 (loop while (> player1-spot 10) do
		   (decf player1-spot 10))

		 (incf player1-score player1-spot)

		 (setf current-player 2))
	  if (and (< player1-score 1000) (= current-player 2))
	    do
	       (let ((steps 0))
		 (incf steps die)

		 (progn
		   (incf die)
		   (when (> die 100)
		     (decf die 100)))

		 (incf steps die)

		 (progn
		   (incf die)
		   (when (> die 100)
		     (decf die 100)))

		 (incf steps die)

		 (progn
		   (incf die)
		   (when (> die 100)
		     (decf die 100)))

		 (incf die-thrown 3)

		 (incf player2-spot steps)
		 (loop while (> player2-spot 10) do
		   (decf player2-spot 10))

		 (incf player2-score player2-spot)

		 (setf current-player 1)))
    (* (min player1-score player2-score) die-thrown)))

(defun get-combination-counts ()
  (let ((ret (make-hash-table :test 'equal)))
    (loop for a from 1 to 3 do
      (loop for b from 1 to 3 do
	(loop for c from 1 to 3 do
	  (incf (gethash (+ a b c) ret 0)))))
    ret))

(defconstant +combinations+ (get-combination-counts))

(fare-memoization:define-memo-function get-win-counts (player
						       
						       player1-pos
						       player1-points

						       player2-pos
						       player2-points)
  (cond
    ((>= player1-points 21)
     (cons 1 0))
    ((>= player2-points 21)
     (cons 0 1))
    ((= player 1)
     (let ((player1-wins 0)
	   (player2-wins 0))
       (loop for throw from 3 to 9 do
	 (let ((new-pos (+ player1-pos throw))
	       (new-points player1-points))
	   (loop while (> new-pos 10) do
	     (decf new-pos 10))
	   (incf new-points new-pos)
	   (destructuring-bind (p1 . p2)
	       (get-win-counts 2 new-pos new-points player2-pos player2-points)
	     (incf player1-wins (* (gethash throw +combinations+) p1))
	     (incf player2-wins (* (gethash throw +combinations+) p2)))))
       (cons player1-wins player2-wins)))
    ((= player 2)
     (let ((player1-wins 0)
	   (player2-wins 0))
       (loop for throw from 3 to 9 do
	 (let ((new-pos (+ player2-pos throw))
	       (new-points player2-points))
	   (loop while (> new-pos 10) do
	     (decf new-pos 10))
	   (incf new-points new-pos)
	   (destructuring-bind (p1 . p2)
	       (get-win-counts 1 player1-pos player1-points new-pos new-points)
	     (incf player1-wins (* (gethash throw +combinations+) p1))
	     (incf player2-wins (* (gethash throw +combinations+) p2)))))
       (cons player1-wins player2-wins)))))

(defun aoc-2021-day21-part2 (input)
  (destructuring-bind (p1 . p2)
      (get-win-counts 1 (car input) 0 (cdr input) 0)
    (max p1 p2)))

(defconstant +input+ (parse-input (alexandria:read-file-into-string "day21-input")))

(format t "Part 1: ~A~%" (aoc-2021-day21-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day21-part2 +input+))
