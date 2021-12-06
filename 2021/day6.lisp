(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)

(defun parse-input (input)
  (mapcar #'parse-integer
	  (str:split-omit-nulls "," input)))

(defun aoc-2021-day6 (input time)
  (let ((current-array (make-array 9 :initial-element 0))
	(new-array (make-array 9 :initial-element 0)))
    ;; Populate initially
    (loop for count in input
	  do (incf (aref current-array count)))
    ;; Iterate through time
    (dotimes (i time)
      ;; Zero out the ``new-array''
      (loop for i from 0 below 9
	    do (setf (aref new-array i) 0))
      ;; And populate again
      (loop for i from 8 downto 1
	    do (setf (aref new-array (1- i))
		     (aref current-array i)))
      ;; Handle time zero specially
      (setf (aref new-array 8) (aref current-array 0))
      (incf (aref new-array 6) (aref current-array 0))
      ;; And copy over to ``current-array''
      (loop for i from 0 below (length new-array)
	    do (setf (aref current-array i) (aref new-array i))))
    (reduce #'+ new-array)))

(defun aoc-2021-day6-part1 (input)
  (aoc-2021-day6 input 80))

(defun aoc-2021-day6-part2 (input)
  (aoc-2021-day6 input 256))

(defconstant +input+ (parse-input
		      (alexandria:read-file-into-string "day6-input")))

(format t "Part 1: ~A~%" (aoc-2021-day6-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day6-part2 +input+))
