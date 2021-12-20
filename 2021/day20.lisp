(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)
(ql:quickload :priority-queue)
(ql:quickload :cl-ppcre)
(ql:quickload :trivial-escapes)

(named-readtables:in-readtable trivial-escapes:readtable)

(defun parse-input (input)
  (destructuring-bind (image-enhancing input) (str:split #"\n\n" input)
    (let ((enhance-ret (make-array 512 :element-type '(unsigned-byte 1))))
      (loop for c across image-enhancing
	    for i from 0
	    do
	       (setf (aref enhance-ret i)
		     (if (char= c #\#) 1 0)))
      (let* ((image-lines (str:lines input))
	     (height (length image-lines))
	     (width (length (car image-lines)))
	     (image (make-array `(,height ,width)
				:element-type '(unsigned-byte 1))))
	(loop for line in image-lines
	      for y from 0 do
		(loop for c across line
		      for x from 0 do
			(setf (aref image y x)
			      (if (char= c #\#) 1 0))))
	(cons enhance-ret image)))))

(defun get-enhance-index (old-image x y light-edges image-enhancing)
  (let ((ret 0)
	(old-width  (array-dimension old-image 1))
	(old-height (array-dimension old-image 0))
	(darkness-lights-up (= (aref image-enhancing 0) 1)))
    (loop for y-off from -1 to 1 do
      (loop for x-off from -1 to 1 do
        (let ((x (1- (+ x x-off)))
	      (y (1- (+ y y-off))))
	  (setf ret
		(logior (ash ret 1)
			(if (or (< x 0) (< y 0)
				(>= x old-width)
				(>= y old-height))
			    (if (and darkness-lights-up light-edges) 1 0)
			    (aref old-image y x)))))))
    ret))

(defun do-enhancement (image-enhancing image iteration)
  (let* ((new-width  (+ (array-dimension image 1) 2))
	 (new-height (+ (array-dimension image 0) 2))
	 (new-image  (make-array `(,new-height ,new-width)
				 :element-type '(unsigned-byte 1)
				 :initial-element 0)))
    (loop for y from 0 below new-height do
      (loop for x from 0 below new-width do
	(setf (aref new-image y x)
	      (aref image-enhancing
		    (get-enhance-index image x y (= (mod iteration 2) 1) image-enhancing)))))
    new-image))

(defun print-image (image)
  (let ((width (array-dimension image 1))
	(height (array-dimension image 0)))
    (loop for y from 0 below height do
      (loop for x from 0 below width do
	    (format t "~A" (if (= (aref image y x) 1) #\# #\.)))
      (format t "~%"))))

(defun aoc-2021-day20 (input enhance-count)
  (let ((image-enhancing (car input))
	(image (cdr input)))
    (dotimes (i enhance-count)
      (setf image (do-enhancement image-enhancing image i)))
    (loop for y from 0 below (array-dimension image 0)
	  summing (loop for x from 0 below (array-dimension image 1)
			summing (aref image y x)))))

(defun aoc-2021-day20-part1 (input)
  (aoc-2021-day20 input 2))

(defun aoc-2021-day20-part2 (input)
  (aoc-2021-day20 input 50))

(defconstant +input+ (parse-input (alexandria:read-file-into-string "day20-input")))

(format t "Part 1: ~A~%" (aoc-2021-day20-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day20-part2 +input+))
