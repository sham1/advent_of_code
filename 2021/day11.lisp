(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)

(defun parse-input (input)
  (let* ((lines (str:lines input))
	 (array (make-array `(,(length lines) ,(length (car lines)))
			   :element-type 'fixnum)))
    (loop for line in lines
	  for y from 0 do
	    (loop for character across line
		  for x from 0 do
		    (setf (aref array y x) (parse-integer (string character)))))
    array))

(defun get-neighbours (x y width height)
  (let ((ret nil))
    (loop for y-prime from -1 to 1 do
      (loop for x-prime from -1 to 1 do
	(when (not (= x-prime y-prime 0))
	  (let ((x-off (+ x x-prime))
		(y-off (+ y y-prime)))
	    (when (not (or (< x-off 0)
			   (< y-off 0)
			   (>= x-off width)
			   (>= y-off height)))
	      (push (cons x-off y-off) ret))))))
    ret))

(defun iterate (input iteration-count)
  (let* ((width (array-dimension input 0))
	 (height (array-dimension input 1))
	 (flashed-octopodes (make-array `(,height ,width)
					:element-type 'boolean
					:initial-element nil))
	 (flashes 0))
    (dotimes (i iteration-count)
      (loop for y from 0 below height do
	(loop for x from 0 below width do
	  (incf (aref input y x))))

      (let ((needs-update t))
	(loop while needs-update do
	  (setf needs-update nil)
	  (loop for y from 0 below height do
	    (loop for x from 0 below width do
	      (when (and (> (aref input y x) 9)
			 (not (aref flashed-octopodes y x)))
		(setf (aref flashed-octopodes y x) t)
		(loop for (x . y) in (get-neighbours x y width height)
		      do (incf (aref input y x))
			 (when (> (aref input y x) 9)
			   (setf needs-update t))))))))

      (loop for y from 0 below height do
	(loop for x from 0 below width
	      when (aref flashed-octopodes y x)
		do (setf (aref flashed-octopodes y x) nil)
		   (setf (aref input y x) 0)
		   (incf flashes))))
    flashes))

(defun find-sync-point (input)
  (let* ((width (array-dimension input 0))
	 (height (array-dimension input 1))
	 (flashed-octopodes (make-array `(,height ,width)
					:element-type 'boolean
					:initial-element nil))
	 (flashes 0)
	 (iteration 0)
	 (found-sync nil))
    (loop until found-sync do
      (loop for y from 0 below height do
	(loop for x from 0 below width do
	  (incf (aref input y x))))

      (let ((needs-update t))
	(loop while needs-update do
	  (setf needs-update nil)
	  (loop for y from 0 below height do
	    (loop for x from 0 below width do
	      (when (and (> (aref input y x) 9)
			 (not (aref flashed-octopodes y x)))
		(setf (aref flashed-octopodes y x) t)
		(loop for (x . y) in (get-neighbours x y width height)
		      do (incf (aref input y x))
			 (when (> (aref input y x) 9)
			   (setf needs-update t))))))))

      (setf found-sync t)
      (loop for y from 0 below height do
	(loop for x from 0 below width
	      if (aref flashed-octopodes y x)
		do (setf (aref flashed-octopodes y x) nil)
		   (setf (aref input y x) 0)
		   (incf flashes)
	      else do (setf found-sync nil)))
      (incf iteration))
    iteration))

(defconstant +input+ (parse-input (alexandria:read-file-into-string "day11-input")))

(defun aoc-2021-day11-part1 (input)
  (let ((copy (alexandria:copy-array input)))
    (iterate copy 100)))

(defun aoc-2021-day11-part2 (input)
  (let ((copy (alexandria:copy-array input)))
    (find-sync-point copy)))

(format t "Part 1: ~A~%" (aoc-2021-day11-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day11-part2 +input+))
