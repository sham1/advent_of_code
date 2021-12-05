(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)

(defun parse-line (line)
  (trivia:match (str:words line)
    ((list begin "->" end)
     (let* ((begin (mapcar #'parse-integer (str:split "," begin)))
	    (end (mapcar #'parse-integer (str:split "," end)))
	    (x1 (car begin))
	    (y1 (cadr begin))
	    (x2 (car end))
	    (y2 (cadr end)))
     (cons (cons x1 y1) (cons x2 y2))))))

(defun generate-lines (begin end &key allow-diagonal)
  (let ((x1 (car begin))
	(y1 (cdr begin))
	(x2 (car end))
	(y2 (cdr end)))
    (cond
      ((= x1 x2)
       (loop for y from (min y1 y2) to (max y1 y2)
	     collecting (cons x1 y)))
      ((= y1 y2)
       (loop for x from (min x1 x2) to (max x1 x2)
	     collecting (cons x y1)))
      (t (when allow-diagonal
	   (if (< y1 y2)
	       (if (< x1 x2)
		   (loop for y from y1 to y2
			 for x from x1 to x2
			 collecting (cons x y))
		   (loop for y from y2 downto y1
			 for x from x2 to x1
			 collecting (cons x y)))
	       (if (< x1 x2)
		   (loop for y from y1 downto y2
			 for x from x1 to x2
			 collecting (cons x y))
		   (loop for y from y2 to y1
			 for x from x2 to x1
			 collecting (cons x y)))))))))

(defconstant +input+ (mapcar #'parse-line
			     (str:lines
			      (alexandria:read-file-into-string "day5-input"))))

(defun aoc-2021-day5-part1 (input)
  (let ((array (make-array '(1000 1000) :initial-element 0)))
   (loop for (begin . end) in input
	 do (loop for (x . y) in (generate-lines begin end)
		  do (incf (aref array x y))))
    (loop for x from 0 below (array-dimension array 0)
	  summing (loop for y from 0 below (array-dimension array 1)
			counting (>= (aref array x y) 2)))))

(defun aoc-2021-day5-part2 (input)
  (let ((array (make-array '(1000 1000) :initial-element 0)))
   (loop for (begin . end) in input
	 do (loop for (x . y) in (generate-lines begin end :allow-diagonal t)
		  do (incf (aref array x y))))
    (loop for x from 0 below (array-dimension array 0)
	  summing (loop for y from 0 below (array-dimension array 1)
			counting (>= (aref array x y) 2)))))

(format t "Part 1: ~A~%" (aoc-2021-day5-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day5-part2 +input+))
