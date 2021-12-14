(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)

(defmacro aif (expr then &optional else)
  `(let ((it ,expr))
     (if it ,then ,else)))

(defmacro awhen (expr &body then)
  `(let ((it ,expr))
     (when it ,@then)))

(defmacro with-sliding-window ((vec window-name &key (window-len) (window-stride 1))
			       &body body)
  (let ((vector (gensym))
	(index-sym (gensym))
	(last-index (gensym)))
    `(let* ((,vector ,vec)
	    (,last-index (- (length ,vector) ,window-len))
	    (,window-name nil))
      (loop for ,index-sym upto ,last-index by ,window-stride
	   do (progn
		(setf ,window-name (subseq ,vector
					   ,index-sym
					   (+ ,index-sym ,window-len)))
		,@body)))))

(defun parse-input (input)
  (destructuring-bind (initial rules)
      (str:split-omit-nulls (format nil "~%~%") input)
    (let ((rules (str:lines rules)))
      (cons initial (loop for rule in rules
			  collect (trivia:match (str:words rule)
				    ((list chars "->" subst)
				     (cons chars (aref subst 0)))))))))

(defun do-step (input rules)
  (let ((output (make-hash-table :test #'equal)))
    (loop for substring being the hash-key using (hash-value count) of input do
      (aif (assoc substring rules :test #'string-equal)
	   (let* ((medial-char (cdr it))
		  (first-half (format nil "~A~A" (aref substring 0) medial-char))
		  (snd-half (format nil "~A~A" medial-char (aref substring 1))))
	     (incf (gethash first-half output 0) count)
	     (incf (gethash snd-half output 0) count))
	   (incf (gethash substring output 0) count)))
    output))

(defun aoc-2021-day14 (input iteration-count)
  (let ((table (make-hash-table :test #'equal))
	(input (car input))
	(rules (cdr input))
	(occurances (make-hash-table :test #'equal)))
    (with-sliding-window (input substring :window-len 2)
      (incf (gethash substring table 0)))
    (dotimes (i iteration-count)
      (setf table (do-step table rules)))  
    ;; We've iterated over the string.
    ;; Now count the characters (also keep in mind that there is overcount)
    (loop for pair being the hash-key using (hash-value count) of table do
      (let ((fst (aref pair 0))
	    (snd (aref pair 1)))
	(incf (gethash fst occurances 0) count)
	(incf (gethash snd occurances 0) count)))
    (loop for occurance-count in (alexandria:hash-table-values occurances)
	  maximizing occurance-count into most-common
	  minimizing occurance-count into least-common
	  finally (return (- (ceiling most-common 2)
			     (ceiling least-common 2))))))

(defun aoc-2021-day14-part1 (input)
  (aoc-2021-day14 input 10))

(defun aoc-2021-day14-part2 (input)
  (aoc-2021-day14 input 40))

(defconstant +input+ (parse-input (alexandria:read-file-into-string "day14-input")))

(format t "Part 1: ~A~%" (aoc-2021-day14-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day14-part2 +input+))
