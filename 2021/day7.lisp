(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)

(defun parse-input (input)
  (map 'vector #'parse-integer
       (str:split-omit-nulls "," input)))

(defun get-fuel-for-part1 (crabs pos)
  (loop for crab across crabs
	summing (abs (- pos crab))))

(defun get-fuel-for-part2 (crabs pos)
  (loop for crab across crabs
	summing (let ((dist (abs (- pos crab))))
		  (/ (* dist (1+ dist)) 2))))

(defun aoc-2021-day7-part1 (input)
  (let* ((crabs (sort input #'<)))
    (loop for pos across crabs
	  minimizing (get-fuel-for-part1 crabs pos))))

(defun aoc-2021-day7-part2 (input)
  (let* ((crabs (sort input #'<)))
    (loop for pos across crabs
	  minimizing (get-fuel-for-part2 crabs pos))))

(defconstant +input+ (parse-input (alexandria:read-file-into-string "day7-input")))

(format t "Part 1: ~A~%" (aoc-2021-day7-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day7-part2 +input+))
