(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)
(ql:quickload :priority-queue)

(defun char-to-integer (char)
  (- (char-code char) (char-code #\0)))

(defun parse-input (input)
  (let* ((lines (str:lines input))
	 (height (length lines))
	 (width (length (car lines)))
	 (grid (make-array `(,height ,width))))
    (loop for line in lines
	  for y from 0
	  do (loop for char across line
		   for x from 0
		   do (setf (aref grid y x) (char-to-integer char))))
    grid))

(defun get-neighbours (x y width height)
  (let ((ret nil))
    (when (> x 0)
      (push (cons (1- x) y) ret))
    (when (< x (1- width))
      (push (cons (1+ x) y) ret))
    (when (> y 0)
      (push (cons x (1- y)) ret))
    (when (< y (1- height))
      (push (cons x (1+ y)) ret))
    ret))

(defun heuristic (x1 y1 x2 y2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defun find-path (input &key (width (array-dimension input 0))
			  (height (array-dimension input 1))
			  (graph-access #'aref))
  "A simple A* implementation that finds the path of lowest risk in `input'."
  (let* ((end-x (1- width))
	 (end-y (1- height))
	 (frontier (priority-queue:make-pqueue #'<))
	 (came-from (make-hash-table :test #'equal))
	 (cost-so-far (make-hash-table :test #'equal)))
    (priority-queue:pqueue-push '(0 . 0) 0 frontier)
    (setf (gethash '(0 . 0) came-from) nil)
    (setf (gethash '(0 . 0) cost-so-far) 0)

    (loop until (priority-queue:pqueue-empty-p frontier) do
      (multiple-value-bind (current priority) (priority-queue:pqueue-pop frontier)
	(declare (ignore priority))
	(when (equalp current `(,end-x . ,end-y))
	  (return))

	(loop for neighbour in (get-neighbours (car current) (cdr current) width height)
	      do (let* ((neighbour-x (car neighbour))
			(neighbour-y (cdr neighbour))
			(new-cost (+ (gethash current cost-so-far)
				     (funcall graph-access
					      input neighbour-y neighbour-x))))
		   (when (or (not (gethash neighbour cost-so-far))
			     (< new-cost (gethash neighbour cost-so-far)))
		     (setf (gethash neighbour cost-so-far) new-cost)
		     (priority-queue:pqueue-push
		      neighbour
		      (+ new-cost (heuristic neighbour-x neighbour-y end-x end-y))
		      frontier)
		     (setf (gethash neighbour came-from) current))))))
    (values came-from cost-so-far)))

(defun aoc-2021-day15-part1 (input)
  (let ((end-x (1- (array-dimension input 0)))
	(end-y (1- (array-dimension input 1))))
    (gethash `(,end-x . ,end-y) (nth-value 1 (find-path input)))))

(defun access-graph-part2 (input y x)
  (let* ((original-width (array-dimension input 0))
	 (original-height (array-dimension input 1))
	 (original-value (aref input
			       (mod y original-height)
			       (mod x original-width)))
	 (adjusted-value (+ original-value
			    (floor y original-height)
			    (floor x original-width))))
    (loop while (> adjusted-value 9) do
	  (decf adjusted-value 9))
    adjusted-value))

(defun aoc-2021-day15-part2 (input)
  (let* ((width (array-dimension input 0))
	 (height (array-dimension input 1))
	 (end-x (1- (* 5 width)))
	 (end-y (1- (* 5 height))))
    (gethash `(,end-x . ,end-y)
	     (nth-value 1
			(find-path input
				   :width (* 5 width)
				   :height (* 5 height)
				   :graph-access #'access-graph-part2)))))

(defconstant +input+ (parse-input (alexandria:read-file-into-string "day15-input")))

(format t "Part 1: ~A~%" (aoc-2021-day15-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day15-part2 +input+))
