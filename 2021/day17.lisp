(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)
(ql:quickload :priority-queue)
(ql:quickload :cl-ppcre)

(defun parse-input (input)
  (trivia:match (cl-ppcre:split "([:,] |=)" input)
    ((list _ _ x _ y)
     (let ((x (str:split ".." x))
	   (y (str:split ".." y)))
       (list (cons (parse-integer (car x)) (parse-integer (cadr x)))
	     (cons (parse-integer (car y)) (parse-integer (cadr y))))))))

(defun find-minimum-triangular-number (min-target)
  ;; We know that the equation of a triangular number is
  ;; (n*(n + 1))/2 = (n²+n)/2, and we want to find smallest n such that
  ;; (n² + n)/2 ≥ min-target <=> n² + n ≥ 2 · min-target
  ;;                         <=> n² + n - 2 · min-target ≥ 0
  ;;
  ;; And we now solve it by the quadratic formula, and take the ceiling of
  ;; the positive result
  (let ((n (/ (+ -1 (sqrt (- 1 (* 4 (- (* 2 min-target)))))) 2)))
    (nth-value 0 (ceiling n))))

(defun find-maximal-triangular-number (min-target)
  ;; We know that the equation of a triangular number is
  ;; (n*(n + 1))/2 = (n²+n)/2, and we want to find largest n such that
  ;; (n² + n)/2 ≤ min-target <=> n² + n ≤ 2 · min-target
  ;;                         <=> n² + n - 2 · min-target ≤ 0
  ;;
  ;; And we now solve it by the quadratic formula, and take the floor of
  ;; the positive result
  (let ((n (/ (+ -1 (sqrt (- 1 (* 4 (- (* 2 min-target)))))) 2)))
    (nth-value 0 (floor n))))

(defun triangular-number (n)
  (/ (* n (+ n 1)) 2))

(defun aoc-2021-day17-part1 (input)
  (let ((target-x-interval (car input))
	(target-y-interval (cadr input))
	(current-apex most-negative-fixnum))
    ;; So, here are the preconditions:
    ;;
    ;; x(0) = 0, y(0) = 0
    ;; x''(t) = -sgn(x'(t)), y''(t) = -1
    ;;
    ;; We need to find x'(0) and y'(0) such that
    ;; ∃t∈N, x(t) ∈ target-x-interval and y(t) ∈ target-y-interval, and
    ;; maximize y(t) when y'(t) = 0.
    ;;
    ;; Because we know that while the |x'(t)| > 0, x'' goes towards zero by one.
    ;; Thus we want to find the smallest triangular number such that
    ;; T_{x_min} >= min(target-x-interval).
    ;; We also want to find the largest triangular number such that
    ;; T_{x_max} <= max(target-x-interval).
    ;;
    ;; Then we can vary y'(0) to find a lob that maximizes y(t).
    (let ((min-xprime (find-minimum-triangular-number
		       (min (abs (car target-x-interval))
			    (abs (cdr target-x-interval)))))
	  (max-xprime (find-maximal-triangular-number
		       (max (abs (car target-x-interval))
			    (abs (cdr target-x-interval))))))
      ;; Because y'' is constant, we know that the apex is going to be
      ;; y'(0)th triangular number
      (loop for x-prime from min-xprime to max-xprime do
	(let ((invalid-count 0))
	  (loop for y-prime from 1 do
	    (let ((y-apex (triangular-number y-prime)))
	      (unless (> y-apex current-apex)
		(return))
	      ;; y-apex is now a potential apex. Now we just
	      ;; need to know if we get into the region
	      (let ((x 0)
		    (y 0)
		    (x-prime x-prime)
		    (y-prime y-prime)
		    (valid nil))
		(loop until (or (> x (max (car target-x-interval)
					  (cdr target-x-interval)))
				(< y (min (car target-y-interval)
					  (cdr target-y-interval))))
		      do
			 (incf x x-prime)
			 (incf y y-prime)
			 (setf x-prime (- x-prime (signum x-prime)))
			 (decf y-prime)

			 (when (and (<= (min (car target-x-interval)
					     (cdr target-x-interval))
					x
					(max (car target-x-interval)
					     (cdr target-x-interval)))
				    (<= (min (car target-y-interval)
					     (cdr target-y-interval))
					y
					(max (car target-y-interval)
					     (cdr target-y-interval))))
			   (setf valid t)))
		;; We now have a valid apex!
		(if valid
		    (progn
		      (setf invalid-count 0)
		      (setf current-apex y-apex))
		    (progn
		      (incf invalid-count)
		      ;; Getting 1000 misses in a row should indicate
		      ;; futility
		      (when (> invalid-count 1000)
			  (return))))))))))
    current-apex))

(defun aoc-2021-day17-part2 (input)
  (let ((target-x-interval (car input))
	(target-y-interval (cadr input))
        (hits nil))
    ;; So, here are the preconditions:
    ;;
    ;; x(0) = 0, y(0) = 0
    ;; x''(t) = -sgn(x'(t)), y''(t) = -1
    ;;
    ;; We need to find x'(0) and y'(0) such that
    ;; ∃t∈N, x(t) ∈ target-x-interval and y(t) ∈ target-y-interval, and
    ;; maximize y(t) when y'(t) = 0.
    ;;
    ;; Because we know that while the |x'(t)| > 0, x'' goes towards zero by one.
    ;; Thus we want to find the smallest triangular number such that
    ;; T_{x_min} >= min(target-x-interval).
    ;; We also want to find the largest triangular number such that
    ;; T_{x_max} <= max(target-x-interval).
    ;;
    ;; Then we can vary y'(0) to find a lob that gets to the target.
    (let ((invalid-count 0))
      (loop for x-prime from 1 to (max (car target-x-interval)
				       (cdr target-x-interval))
	    do
	       (let ((x-hit nil)
		     (y-invalid-count 0))
		 (loop for y-prime from (min (car target-y-interval)
					     (cdr target-y-interval))
		       do
			  ;; Now we just need to know if we get into the region
			  (let ((x-prime-zero x-prime)
				(y-prime-zero y-prime)
				(x 0)
				(y 0)
				(x-prime x-prime)
				(y-prime y-prime)
				(valid nil))
			    (loop until (or (> x (max (car target-x-interval)
						      (cdr target-x-interval)))
					    (< y (min (car target-y-interval)
						      (cdr target-y-interval))))
				  do
				     (incf x x-prime)
				     (incf y y-prime)
				     (setf x-prime (- x-prime (signum x-prime)))
				     (decf y-prime)

				     (when (and (<= (min (car target-x-interval)
							 (cdr target-x-interval))
						    x
						    (max (car target-x-interval)
							 (cdr target-x-interval)))
						(<= (min (car target-y-interval)
							 (cdr target-y-interval))
						    y
						    (max (car target-y-interval)
							 (cdr target-y-interval))))
				       (setf valid t)))
			    ;; We now have a valid apex!
			    (if valid
				(progn
				  (setf x-hit t)
				  (setf y-invalid-count 0)
				  (pushnew (cons x-prime-zero y-prime-zero) hits
					   :test #'equalp))
				(progn
				  (incf y-invalid-count)
				  ;; Getting 2500 misses in a row should indicate
				  ;; futility
				  (when (> y-invalid-count 2500)
				    (return))))))
		 (unless x-hit
		   (incf invalid-count)
		   (when (> invalid-count 1000)
		     (return))))))
    (length hits)))

(defconstant +input+ (parse-input
		      (car
		       (str:lines
			(alexandria:read-file-into-string "day17-input")))))

(format t "Part 1: ~A~%" (aoc-2021-day17-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day17-part2 +input+))
