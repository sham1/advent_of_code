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

(defun parse-input (input)
  (let* ((lines (str:lines input))
	 (height (length lines))
	 (width (length (car lines)))

	 (horizontals (hash-set:make-hash-set))
	 (verticals (hash-set:make-hash-set)))
    (iterate:iter
      (iterate:for line in lines)
      (iterate:for y from 0)

      (iterate:iter
	(iterate:for c in-string line)
	(iterate:for x from 0)

        (cond
	  ((char= c #\>) (hash-set:hs-ninsert horizontals (cons x y)))
	  ((char= c #\v) (hash-set:hs-ninsert verticals (cons x y))))))
    (list width height horizontals verticals)))

(defun aoc-2021-day25-part1 (input)
  (destructuring-bind (width height initial-horizontals initial-verticals) input
    (let ((horizontals initial-horizontals)
	  (verticals initial-verticals)
	  (new-horizontals nil)
	  (new-verticals nil)

	  (first-iter t)
	  (moved nil)
	  (iter-count 0))
      (loop while (or first-iter moved) do
	(setf moved nil
	      new-horizontals (hash-set:make-hash-set)
	      new-verticals (hash-set:make-hash-set))

	;; First, simulate all the horizontals
	(hash-set:dohashset (cucumber horizontals)
	  (let* ((x (car cucumber))
		 (y (cdr cucumber))
		 (new-x (mod (1+ x) width))
		 (new-coords (cons new-x y)))
	    (if (or (hash-set:hs-memberp horizontals new-coords)
		    (hash-set:hs-memberp verticals new-coords))
		(hash-set:hs-ninsert new-horizontals cucumber)
		(progn
		  (hash-set:hs-ninsert new-horizontals new-coords)
		  (setf moved t)))))

	;; Then the verticals
	(hash-set:dohashset (cucumber verticals)
	  (let* ((x (car cucumber))
		 (y (cdr cucumber))
		 (new-y (mod (1+ y) height))
		 (new-coords (cons x new-y)))
	    (if (or (hash-set:hs-memberp new-horizontals new-coords)
		    (hash-set:hs-memberp verticals new-coords))
		(hash-set:hs-ninsert new-verticals cucumber)
		(progn
		  (hash-set:hs-ninsert new-verticals new-coords)
		  (setf moved t)))))

	(setf first-iter nil
	      horizontals new-horizontals
	      verticals new-verticals)
	(incf iter-count))
      iter-count)))

(defconstant +input+ (parse-input (alexandria:read-file-into-string "day25-input")))

(format t "Part 1: ~A~%" (aoc-2021-day25-part1 +input+))
