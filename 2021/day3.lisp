(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)

(defun find-gamma-epsilon (input bit-count)
  (let ((gamma 0)
	(epsilon 0))
    (loop for i from (1- bit-count) downto 0
	  do (let ((ones 0)
		   (zeroes 0))
	       (loop for n in input
		     do (let ((bit (logand (ash n (- i)) 1)))
			  (if (= bit 1) (incf ones) (incf zeroes))))
	       (cond
		 ((> zeroes ones)
		  (setf gamma (logior (ash gamma 1) 0))
		  (setf epsilon (logior (ash epsilon 1) 1)))
		 ((> ones zeroes)
		  (setf gamma (logior (ash gamma 1) 1))
		  (setf epsilon (logior (ash epsilon 1) 0))))))
    (values gamma epsilon)))

(defun find-oxygen-co2 (input bit-count)
  (let ((oxygen 0)
	(co2 0)
	(oxygen-list input)
	(co2-list input))
    (setf oxygen
	  (loop for i from (1- bit-count) downto 0
		do (let ((ones 0)
			 (zeroes 0)
			 (ones-list nil)
			 (zeroes-list nil))
		     (loop for n in oxygen-list
			   do (let ((bit (logand (ash n (- i)) 1)))
				(cond
				  ((= bit 1)
				   (incf ones)
				   (push n ones-list))
				  (t (incf zeroes)
				     (push n zeroes-list)))))
		     (cond
		       ((< ones zeroes) (setf oxygen-list zeroes-list))
		       (t (setf oxygen-list ones-list)))
		     (when (= (length oxygen-list) 1)
		       (return (car oxygen-list))))))
    (setf co2
	  (loop for i from (1- bit-count) downto 0
		do (let ((ones 0)
			 (zeroes 0)
			 (ones-list nil)
			 (zeroes-list nil))
		     (loop for n in co2-list
			   do (let ((bit (logand (ash n (- i)) 1)))
				(cond
				  ((= bit 1)
				   (incf ones)
				   (push n ones-list))
				  (t (incf zeroes)
				     (push n zeroes-list)))))
		     (cond
		       ((< ones zeroes) (setf co2-list ones-list))
		       (t (setf co2-list zeroes-list)))
		     (when (= (length co2-list) 1)
		       (return (car co2-list))))))
    (values oxygen co2)))

(defconstant +input+ (mapcar (lambda (s) (parse-integer s :radix 2))
			     (str:lines
			      (alexandria:read-file-into-string "day3-input"))))

(defun aoc-2021-day3-part1 (input)
  (multiple-value-call #'* (find-gamma-epsilon input 12)))

(defun aoc-2021-day3-part2 (input)
  (multiple-value-call #'* (find-oxygen-co2 input 12)))

(format t "Part 1: ~A~%" (aoc-2021-day3-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day3-part2 +input+))

