(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)

(defun parse-line (line)
  (let ((words (str:words line)))
    (cons (subseq words 0 10)
	  (subseq words 11))))

(defun unique-number-p (output)
  (let ((length (length output)))
    (cond
      ((= length 2) 1)
      ((= length 4) 4)
      ((= length 3) 7)
      ((= length 7) 8)
      (t nil))))

(defconstant +input+ (mapcar #'parse-line
			     (str:lines (alexandria:read-file-into-string "day8-input"))))

(defun aoc-2021-day8-part1 (input)
  (loop for (inputs . outputs) in input
        summing (loop for output in outputs
		      count (unique-number-p output))))

(defun sort-strings (list)
  (mapcar (lambda (s)
	    (sort s #'char<=))
	  list))

(defun charset-xor (&rest args)
  (let ((ret (list)))
    (loop for str in args
	  do (let* ((chars (coerce str 'list))
		    (intersection (intersection ret chars))
		    (ret-difference (set-difference ret intersection))
		    (chars-difference (set-difference chars intersection))
		    (uniques (union ret-difference chars-difference)))
	       (setf ret uniques)))
    (coerce (sort ret #'char<) 'string)))

(defun charset-minus (seed &rest rest)
  (let ((ret (coerce seed 'list)))
    (loop for str in rest
	  do (setf ret (set-difference ret (coerce str 'list))))
    (coerce (sort ret #'char<) 'string)))

(defun charset-and (&rest args)
  (when args
    (let ((ret (coerce (car args) 'list)))
      (loop for str in (cdr args)
	    do (setf ret (intersection ret (coerce str 'list))))
      (coerce (sort ret #'char<) 'string))))

(defun charset-or (&rest args)
  (let ((ret (list)))
    (loop for str in args
	  do (setf ret (union ret (coerce str 'list))))
    (coerce (sort ret #'char<) 'string)))

(defun permutation-proper-p (inputs perm)
  (destructuring-bind (a b c d e f g) (coerce perm 'list)
    (loop for input in inputs
	  always (cond
		   ((and (= (length input) 2)
			 (not (every (lambda (char) (member char `(,c ,f))) input)))
		    nil)
		   ((and (= (length input) 3)
			 (not (every (lambda (char) (member char `(,a ,c ,f))) input)))
		    nil)
		   ((and (= (length input) 4)
			 (not (every (lambda (char) (member char `(,b ,c ,d ,f))) input)))
		    nil)
		   ((and (= (length input) 5)
			 (not (every (lambda (char) (member char `(,a ,c ,d ,e ,g))) input))
			 (not (every (lambda (char) (member char `(,a ,c ,d ,f ,g))) input))
			 (not (every (lambda (char) (member char `(,a ,b ,d ,f ,g))) input)))
		    nil)
		   ((and (= (length input) 6)
			 (not (every (lambda (char) (member char `(,a ,b ,c ,e ,f ,g))) input))
			 (not (every (lambda (char) (member char `(,a ,b ,d ,e ,f ,g))) input))
			 (not (every (lambda (char) (member char `(,a ,b ,c ,d ,f ,g))) input)))
		    nil)
		   (t t)))))

(defun find-permutation (input)
  (let ((inputs (sort-strings (car input)))
	(ret (list)))
    (alexandria:map-permutations
     (lambda (perm)
       (when (permutation-proper-p inputs perm)
	 (push perm ret)))
     "abcdefg")
    (car ret)))

(defun apply-permutation (orig permutation)
  (coerce (loop for c across orig
		collect (aref permutation (- (char-code c) (char-code #\a))))
	  'string))

(defconstant +numbers+
  #("abcefg" "cf" "acdeg" "acdfg" "bcdf"
    "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"))

(defun get-output-sum (input)
  (let* ((ret 0)
	 (outputs (sort-strings (cdr input)))
	 (permutation (find-permutation input))
	 (transformed-numbers (coerce
			       (sort-strings
				(map 'list (lambda (s)
					     (apply-permutation s permutation))
				     +numbers+))
			       'vector)))
    (loop for output in outputs	  
	  do (loop for number from 0 to 9
		   when (string= output (aref transformed-numbers number))
		     do (setf ret (+ (* ret 10) number))))
    ret))

(defun aoc-2021-day8-part2 (inputs)
  (loop for input in inputs
	summing (get-output-sum input)))

(format t "Part 1: ~A~%" (aoc-2021-day8-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day8-part2 +input+))
