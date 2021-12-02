(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)

(defun parse-command (line)
  (let ((parts (str:words line)))
    (cons (intern (string-upcase (car parts))) (parse-integer (cadr parts)))))

(defun aoc-2021-day2-part1 (input)
  (let ((cmds (mapcar #'parse-command input))
	(pos 0)
	(depth 0))
    (loop for (cmd . count) in cmds
	  do (ecase cmd
	       (forward (incf pos count))
	       (up (decf depth count))
	       (down (incf depth count))))
    (* pos depth)))

(defun aoc-2021-day2-part2 (input)
  (let ((cmds (mapcar #'parse-command input))
	(pos 0)
	(depth 0)
	(aim 0))
    (loop for (cmd . count) in cmds
	  do (ecase cmd
	       (forward
		(incf pos count)
		(incf depth (* aim count)))
	       (up (decf aim count))
	       (down (incf aim count))))
    (* pos depth)))

(defconstant +input+ (str:lines (alexandria:read-file-into-string "day2-input")))

(format t "Part 1: ~A~%" (aoc-2021-day2-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day2-part2 +input+))
