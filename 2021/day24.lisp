(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)
(ql:quickload :priority-queue)
(ql:quickload :cl-ppcre)
(ql:quickload :trivial-escapes)
(ql:quickload :hash-set)
(ql:quickload :fare-memoization)

(named-readtables:in-readtable trivial-escapes:readtable)

(defstruct rule i j addend)

(defconstant +max-model-number-index+ 13)

(defun parse-rules (input)
  (let ((rules (make-array 1 :adjustable t :fill-pointer 0))
	(stack nil))
    (let* ((blocks (str:split-omit-nulls #"\ninp w" input))
	   (nums (iterate:iter (iterate:for block in blocks)
		   (iterate:collect
		       (iterate:iter
			 (iterate:for d in (cl-ppcre:all-matches-as-strings
					    "-?\\d+" block))
			 (iterate:collect (parse-integer d) result-type 'vector))))))
      (iterate:iter (iterate:for ns in nums) (iterate:for j from 0)
        (let ((a (aref ns 2))
	      (b (aref ns 3))
	      (c (aref ns 9)))
	  (if (= a 1)
	      (setf stack (cons `(,c . ,j) stack))
	      (progn
		(destructuring-bind (c . i) (car stack)
		  (setf stack (cdr stack))
		  (let ((addend (+ b c)))
		    (vector-push-extend (make-rule :i i :j j :addend addend) rules))))))))
    (sort rules (lambda (a b) (> (rule-i a) (rule-i b))))))

(defun aoc-2021-day24-part1 (input)
  (let ((model-number 0)
	(rules (parse-rules input)))
    (iterate:iter (iterate:for rule in-vector rules)
      (with-slots (i j addend) rule
	(let* ((pair (if (> addend 0) (cons (- 9 addend) 9) (cons 9 (+ 9 addend))))
	       (a (car pair))
	       (b (cdr pair)))
	  (incf model-number (* a (expt 10 (- +max-model-number-index+ i))))
	  (incf model-number (* b (expt 10 (- +max-model-number-index+ j)))))))
    model-number))

(defun aoc-2021-day24-part2 (input)
  (let ((model-number 0)
	(rules (parse-rules input)))
    (iterate:iter (iterate:for rule in-vector rules)
      (with-slots (i j addend) rule
	(let* ((pair (if (> addend 0) (cons 1 (+ 1 addend)) (cons (- 1 addend) 1)))
	       (a (car pair))
	       (b (cdr pair)))
	  (incf model-number (* a (expt 10 (- +max-model-number-index+ i))))
	  (incf model-number (* b (expt 10 (- +max-model-number-index+ j)))))))
    model-number))

(defconstant +input+ (alexandria:read-file-into-string "day24-input"))

(format t "Part 1: ~14,'0D~%" (aoc-2021-day24-part1 +input+))
(format t "Part 2: ~14,'0D~%" (aoc-2021-day24-part2 +input+))
