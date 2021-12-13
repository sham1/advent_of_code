(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)

(defun parse-input (input)
  (trivia:match (str:split (format nil "~%~%") input)
    ((list positions folds)
     (let ((positions (str:lines positions))
	   (folds (str:lines folds))
	   (width 0)
	   (height 0))
       (list (mapcar (lambda (coord)
		       (trivia:match (str:split "," coord)
			 ((list x y)
			  (let ((x (parse-integer x))
				(y (parse-integer y)))
			    (when (> x width)
			      (setf width x))
			    (when (> y height)
			      (setf height y))
			    (cons x y)))))
		     positions)
	     (mapcar (lambda (fold)
		       (trivia:match (str:words fold)
			 ((list _ _ inst)
			  (let ((split (str:split "=" inst)))
			    (cons (intern (str:upcase (car split)))
				  (parse-integer (cadr split)))))))
		     folds)
	     width
	     height)))))

(defconstant +input+ (parse-input (alexandria:read-file-into-string "day13-input")))

(defun remove-dups (elems)
  (let ((new-elems nil))
    (loop for elem in elems do
	  (setf new-elems (union new-elems `(,elem) :test #'equalp)))
    new-elems))

(defun do-fold (dots fold)
  (let ((fold-dir (car fold))
	(fold-coord (cdr fold))
	(new-set nil))
    (setf new-set (loop for (x . y) in dots
			collect (ecase fold-dir
				  (x
				   (if (> x fold-coord)
				       `(,(- fold-coord (- x fold-coord)) . ,y)
				       `(,x . ,y)))
				  (y
				   (if (> y fold-coord)
				       `(,x . ,(- fold-coord (- y fold-coord)))
				       `(,x . ,y))))))
    
    (remove-dups new-set)))

(defun aoc-2021-day13-part1 (input)
  (length (do-fold (car input) (caadr input))))

(defun aoc-2021-day13-part2 (input)
  (let ((dots (car input)))
    (loop for inst in (cadr input) do
      (setf dots (do-fold dots inst)))
  (destructuring-bind (max-x max-y)
      (loop for (x . y) in dots
	    maximize x into max-x
	    maximize y into max-y
	    finally (return (list max-x max-y)))
    (let ((arr (make-array `(,(1+ max-y) ,(1+ max-x )) :initial-element nil)))
      (loop for (x . y) in dots do
	    (setf (aref arr y x) t))
      (loop for y from 0 to max-y do
	(loop for x from 0 to max-x do
	  (format t "~A" (if (aref arr y x) "#" ".")))
	(format t "~%"))))))

(format t "Part 1: ~A~%" (aoc-2021-day13-part1 +input+))
(progn
  (format t "Part 2:~%")
  (aoc-2021-day13-part2 +input+))
