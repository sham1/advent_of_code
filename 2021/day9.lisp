(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)

(defun parse-input (input)
  (let* ((height (length input))
	 (width (length (car input)))
	 (map (make-array `(,height ,width))))
    (loop for line in input
	  for y from 0
	  do (loop for number across line
		   for x from 0
		   do (setf (aref map y x)
			    (parse-integer (string number)))))
    map))

(defun get-neighbors-for (x y width height)
  (let ((ret (list)))
    (unless (= x 0)
      (push (cons (1- x) y) ret))
    (unless (= x (1- width))
      (push (cons (1+ x) y) ret))
    (unless (= y 0)
      (push (cons x (1- y)) ret))
    (unless (= y (1- height))
      (push (cons x (1+ y)) ret))
    ret))

(defun lowest-point-p (input x y width height)
  (let ((current (aref input y x))
	(neighbours (get-neighbors-for x y width height)))
    (= (loop for (neighbour-x . neighbour-y) in neighbours
	     counting (< current (aref input neighbour-y neighbour-x)))
       (length neighbours))))

(defun aoc-2021-day9-part1 (input)
  (let ((width (array-dimension input 1))
	(height (array-dimension input 0)))
  (loop for y from 0 below height
	summing (loop for x from 0 below width
		      summing (if (lowest-point-p input x y width height)
				  (1+ (aref input y x))
				  0)))))

(defun get-basin-size (input x y)
  (let ((width (array-dimension input 1))
	(height (array-dimension input 0))
	(basin (make-hash-table :test 'equal))
	(stack (list (cons x y))))
    (loop if (and (not (null stack)) (not (gethash (car stack) basin)))
	    do (let ((x (caar stack))
		     (y (cdar stack)))
		 (setf stack (cdr stack))
		 (setf (gethash (cons x y) basin) t)
		 (loop for (neighbour-x . neighbour-y) in (get-neighbors-for
							   x
							   y
							   width
							   height)
		       when (and (not (gethash (cons neighbour-x neighbour-y) basin))
				 (< (aref input neighbour-y neighbour-x) 9))
			 do (push (cons neighbour-x neighbour-y) stack)))
	  else
	    do (if (not (null stack))
		   (setf stack (cdr stack))
		   (return)))
    (hash-table-count basin)))

(defun aoc-2021-day9-part2 (input)
  (let* ((width (array-dimension input 1))
	 (height (array-dimension input 0))
	 (low-points
	   (alexandria:plist-alist
	    (alexandria:flatten
	     (loop for y from 0 below height
		   collect (loop for x from 0 below width
				 when (lowest-point-p input x y width height)
				   collect (cons x y))))))
	 (sizes (list)))
    (loop for (x . y) in low-points
	  do (let ((size (get-basin-size input x y)))
	       (push size sizes)))
    (apply #'* (subseq (sort sizes #'>) 0 3))))

(defconstant +input+ (parse-input (str:lines (alexandria:read-file-into-string "day9-input"))))

(format t "Part 1: ~A~%" (aoc-2021-day9-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day9-part2 +input+))
