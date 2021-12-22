(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)
(ql:quickload :priority-queue)
(ql:quickload :cl-ppcre)
(ql:quickload :trivial-escapes)
(ql:quickload :hash-set)

(named-readtables:in-readtable trivial-escapes:readtable)

(defstruct cuboid
  (x1 0)
  (y1 0)
  (z1 0)

  (x2 0)
  (y2 0)
  (z2 0))

(defun parse-line (line)
  (destructuring-bind (state coords) (str:split " " line)
    (let ((state (intern (str:upcase state))))
      (destructuring-bind (x y z) (str:split "," coords)
	(let ((x (mapcar #'parse-integer (str:split ".." (subseq x 2))))
	      (y (mapcar #'parse-integer (str:split ".." (subseq y 2))))
	      (z (mapcar #'parse-integer (str:split ".." (subseq z 2)))))
	  (list state (make-cuboid :x1 (car x)
				   :y1 (car y)
				   :z1 (car z)

				   :x2 (cadr x)
				   :y2 (cadr y)
				   :z2 (cadr z))))))))

(defun cuboid-volume (cuboid)
  (* (1+ (- (cuboid-x2 cuboid) (cuboid-x1 cuboid)))
     (1+ (- (cuboid-y2 cuboid) (cuboid-y1 cuboid)))
     (1+ (- (cuboid-z2 cuboid) (cuboid-z1 cuboid)))))

(defun get-overlap-cuboid (a b)
  (let ((x1 (max (cuboid-x1 a)
		 (cuboid-x1 b)))
	(y1 (max (cuboid-y1 a)
		 (cuboid-y1 b)))
	(z1 (max (cuboid-z1 a)
		 (cuboid-z1 b)))

	(x2 (min (cuboid-x2 a)
		 (cuboid-x2 b)))
	(y2 (min (cuboid-y2 a)
		 (cuboid-y2 b)))
	(z2 (min (cuboid-z2 a)
		 (cuboid-z2 b))))
    (make-cuboid :x1 x1 :y1 y1 :z1 z1
		 :x2 x2 :y2 y2 :z2 z2)))

(defun line-overlap (a1 a2 b1 b2)
  (cons (max a1 b1) (min a2 b2)))

(defun calculate-overlap (cuboid cuboids)
  (loop for existing-cuboid in cuboids
	for index from 0
	summing (let ((overlap-x (line-overlap (cuboid-x1 cuboid)
					       (cuboid-x2 cuboid)
					       (cuboid-x1 existing-cuboid)
					       (cuboid-x2 existing-cuboid)))
		      (overlap-y (line-overlap (cuboid-y1 cuboid)
					       (cuboid-y2 cuboid)
					       (cuboid-y1 existing-cuboid)
					       (cuboid-y2 existing-cuboid)))
		      (overlap-z (line-overlap (cuboid-z1 cuboid)
					       (cuboid-z2 cuboid)
					       (cuboid-z1 existing-cuboid)
					       (cuboid-z2 existing-cuboid))))
		  (if (and (>= (- (cdr overlap-x) (car overlap-x)) 0)
			   (>= (- (cdr overlap-y) (car overlap-y)) 0)
			   (>= (- (cdr overlap-z) (car overlap-z)) 0))
		      (let ((tmp (make-cuboid :x1 (car overlap-x) :x2 (cdr overlap-x)
					      :y1 (car overlap-y) :y2 (cdr overlap-y)
					      :z1 (car overlap-z) :z2 (cdr overlap-z))))
			(- (cuboid-volume tmp)
			   (calculate-overlap tmp (subseq cuboids (1+ index)))))
		      0))))

(defun do-step-part1 (step state)
  (let ((ret (car state))
	(state (cdr state)))
    (destructuring-bind (command cuboid) step 
      (with-slots (x1 x2 y1 y2 z1 z2) cuboid
	(if (cuboid-overlap-p (make-cuboid :x1 -50 :y1 -50 :z1 -50
					   :x2 50 :y2 50 :z2 50)
			      cuboid)
	    (progn
	      (let ((volume (cuboid-volume cuboid)))
		(decf volume (calculate-overlap cuboid (alexandria:hash-table-keys state)))
		(when (< volume 0)
		  (setf volume 0))
		(setf (gethash cuboid state) volume)
		(if (eq command 'on)
		    (cons (+ ret volume) state)
		    (cons ret state))))
	    (cons ret state))))))

(defun do-step-part2 (step state)
  (let ((ret (car state))
	(state (cdr state)))
    (destructuring-bind (command cuboid) step 
      (with-slots (x1 x2 y1 y2 z1 z2) cuboid
	(let ((volume (cuboid-volume cuboid)))
	  (decf volume (calculate-overlap cuboid (alexandria:hash-table-keys state)))
	  (when (< volume 0)
	    (setf volume 0))
	  (setf (gethash cuboid state) volume)
	  (if (eq command 'on)
	      (cons (+ ret volume) state)
	      (cons ret state)))))))

(defun aoc-2021-day22-part1 (input)
  (car (reduce #'do-step-part1 input
	       :initial-value (cons 0 (make-hash-table :test 'equal))
	       :from-end t)))

(defun aoc-2021-day22-part2 (input)
  (car (reduce #'do-step-part2 input
	       :initial-value (cons 0 (make-hash-table :test 'equal))
	       :from-end t)))

(defconstant +input+ (mapcar #'parse-line (str:lines (alexandria:read-file-into-string "day22-input"))))

(format t "Part 1: ~A~%" (aoc-2021-day22-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day22-part2 +input+))
