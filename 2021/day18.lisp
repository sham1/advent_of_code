(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)
(ql:quickload :priority-queue)
(ql:quickload :cl-ppcre)

(defun parse-pair (pair &optional (start 0) (end (length pair)))
  (let ((car nil)
	(cdr nil)
	(car-end nil)
	(cdr-end nil))
    (if (char= (aref pair (1+ start)) #\[)
        (multiple-value-bind (value end-pos)
	    (parse-pair pair (1+ start) end)
	  (setf car value)
	  (setf car-end end-pos))
	(multiple-value-bind (value end-pos)
	    (parse-integer pair :start (1+ start) :junk-allowed t)
	  (setf car value)
	  (setf car-end end-pos)))
    (if (char= (aref pair (1+ car-end)) #\[)
	(multiple-value-bind (value end-pos)
	    (parse-pair pair (1+ car-end) end)
	  (setf cdr value)
	  (setf cdr-end end-pos))
	(multiple-value-bind (value end-pos)
	    (parse-integer pair :start (1+ car-end) :junk-allowed t)
	  (setf cdr value)
	  (setf cdr-end end-pos)))

    (values (cons car cdr) (1+ cdr-end))))

(defun snailfish-number-string (pair)
  (if (integerp pair)
      (format nil "~A" pair)
      (format nil "[~A,~A]"
	      (snailfish-number-string (car pair))
	      (snailfish-number-string (cdr pair)))))

(defun needs-reduction-p (pair)
  (let ((stack nil)
	(has-explosion nil)
	(has-split nil))
    (push (cons pair 1) stack)
    (loop until (null stack) do
      (let ((subpair (caar stack))
	    (depth (cdar stack)))
	(setf stack (cdr stack))

	(when (and (= depth 5) (consp subpair))
	  (setf has-explosion t))

	(when (and (integerp subpair) (>= subpair 10))
	  (setf has-split t))

	(unless (integerp subpair)
	  (push (cons (cdr subpair) (1+ depth)) stack)
	  (push (cons (car subpair) (1+ depth)) stack))))
    (cond
      (has-explosion 'explode)
      (has-split 'split))))

(defun explode-pair (pair)
  ;; First, we find the offending subpair
  (let ((stack nil)
	(parent-pair nil)
	(offending-pair nil)
	(offending-pair-pos nil) ; Either 'car, or 'cdr
	(left-regular-number-pos nil) ; Either 'car, 'cdr, or nil
	(left-regular-number-cons nil)
	(right-number-stack nil))
    (push (list pair nil nil 1) stack)
    (loop until (null stack) do
      (destructuring-bind (subpair parent pos depth) (car stack)
	(setf stack (cdr stack))

	(when (and (= depth 5) (consp subpair))
	  (setf parent-pair parent)
	  (setf offending-pair subpair)
	  (setf offending-pair-pos pos)
	  (return))

	(unless (integerp subpair)
	  (push (list (cdr subpair) subpair 'cdr (1+ depth)) stack)
	  (push (list (car subpair) subpair 'car (1+ depth)) stack))))
    (setf right-number-stack stack)
    (setf stack nil)

    ;; Now, find the last regular number before the offending pair
    (push (list pair nil nil 1) stack)
    (loop until (eql (caar stack) offending-pair) do
      (destructuring-bind (subpair parent pos depth) (car stack)
	(setf stack (cdr stack))

	(when (integerp subpair)
	  (setf left-regular-number-cons parent)
	  (setf left-regular-number-pos pos))

	(unless (integerp subpair)
	  (push (list (cdr subpair) subpair 'cdr (1+ depth)) stack)
	  (push (list (car subpair) subpair 'car (1+ depth)) stack))))

    (when left-regular-number-pos
      (case left-regular-number-pos
	(car (incf (car left-regular-number-cons) (car offending-pair)))
	(cdr (incf (cdr left-regular-number-cons) (car offending-pair)))))

    ;; Now, find the first regular number to the right of the offending pair
    (loop until (null right-number-stack) do
	  (destructuring-bind (subpair parent pos depth) (car right-number-stack)
	    (setf right-number-stack (cdr right-number-stack))

	    (when (integerp subpair)
	      (ecase pos
		(car (incf (car parent) (cdr offending-pair)))
		(cdr (incf (cdr parent) (cdr offending-pair))))
	      (return))

	    (unless (integerp subpair)
	      (push (list (cdr subpair) subpair 'cdr (1+ depth)) right-number-stack)
	      (push (list (car subpair) subpair 'car (1+ depth)) right-number-stack))))
    (ecase offending-pair-pos
      (car (setf (car parent-pair) 0))
      (cdr (setf (cdr parent-pair) 0))))
  pair)

(defun split-pair (pair)
  (let ((stack nil))
    (push (list pair nil nil 1) stack)
    (loop until (null stack) do
      (destructuring-bind (subpair parent pos depth) (car stack)
	(setf stack (cdr stack))

	(when (and (integerp subpair) (>= subpair 10))
	  (let ((split (cons (floor subpair 2)
			     (ceiling subpair 2))))
	    (case pos
	      (car (setf (car parent) split))
	      (cdr (setf (cdr parent) split))))
	  (return))

	(unless (integerp subpair)
	  (push (list (cdr subpair) subpair 'cdr (1+ depth)) stack)
	  (push (list (car subpair) subpair 'car (1+ depth)) stack))))))

(defun reduce-pair (pair)
  (loop do
    (let ((needs-reduction (needs-reduction-p pair)))
      (if needs-reduction
	  (case needs-reduction
	    (explode (explode-pair pair))
	    (split (split-pair pair)))
	  (return pair))))
  pair)

(defun add-pair (a b)
  (let ((new-pair (cons a b)))
    (reduce-pair new-pair)))

(defun sum-pair (pair)
  (if (integerp pair)
      pair
      (+ (* 3 (sum-pair (car pair)))
	 (* 2 (sum-pair (cdr pair))))))

(defconstant +input+ (str:lines (alexandria:read-file-into-string "day18-input")))

(defun aoc-2021-day18-part1 (input)
  (let ((pairs (mapcar #'parse-pair input)))
    (sum-pair
     (reduce (lambda (a b) (add-pair (copy-tree a) (copy-tree b))) pairs))))

(defun aoc-2021-day18-part2 (input)
  (let ((pairs (mapcar #'parse-pair input)))
    (loop for a in pairs
	  maximizing
	  (loop for b in pairs
		unless (eql a b)
		  maximizing (sum-pair (add-pair (copy-tree a)
						 (copy-tree b)))))))

(format t "Part 1: ~A~%" (aoc-2021-day18-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day18-part2 +input+))
