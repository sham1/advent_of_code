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

(defun parse-input (input)
  (let ((scanners (str:split #"\n\n" input)))
    (iterate:iter (iterate:for scanner in scanners)
      (iterate:collecting
       (loop for beacon in (cdr (str:lines scanner))
	     collect (map 'vector #'parse-integer (str:split "," beacon)))
       result-type vector))))

(defun print-matrix (m)
  (loop for i from 0 to 2 do
    (loop for j from 0 to 2 do
      (format t "~A~A" (if (zerop j) "" " ") (aref m j i)))
    (format t "~%")))

(defun matrix-cols (mat)
  (loop for i from 0 below (array-dimension mat 0)
	collect (loop for j from 0 below (array-dimension mat 1)
		      collect (aref mat i j))))

(defun matrix-rows (mat)
  (loop for i from 0 below (array-dimension mat 1)
	collect (loop for j from 0 below (array-dimension mat 0)
		      collect (aref mat j i))))

(defun matrix-vec-* (mat vec)
  (let ((ret (make-array 3 :initial-element 0)))
    (loop for col in (matrix-cols mat)
	  for i from 0 do
	    (loop for n in col
		  for j from 0 do
		    (incf (aref ret j) (* n (aref vec i)))))
    ret))

(defun matrix-* (a b)
  (let ((ret (make-array '(3 3))))
    (loop for column in (matrix-cols b)
	  for column-id from 0 do
	    (loop for row across (matrix-vec-* a (coerce column 'vector))
		  for row-id from 0 to 2 do
		  (setf (aref ret column-id row-id) row)))
    ret))

(defun vector-- (a b)
  (let ((ret (make-array 3)))
    (loop for a across a
	  for b across b
	  for i from 0 do
	    (setf (aref ret i) (- a b)))
    ret))

(defun vector-+ (a b)
  (let ((ret (make-array 3)))
    (loop for a across a
	  for b across b
	  for i from 0 do
	    (setf (aref ret i) (+ a b)))
    ret))

(defconstant +identity+ (make-array
			 '(3 3) :initial-contents
			 '((1 0 0) (0 1 0) (0 0 1))))

(defconstant +x-rot+ (make-array
		      '(3 3) :initial-contents
		      '((1 0 0) (0 0 1) (0 -1 0))))

(defconstant +x-inv-rot+ (make-array
			  '(3 3) :initial-contents
			  '((1 0 0) (0 0 -1) (0 1 0))))

(defconstant +y-rot+ (make-array
		      '(3 3) :initial-contents
		      '((0 0 -1) (0 1 0) (1 0 0))))

(defconstant +y-inv-rot+ (make-array
			  '(3 3) :initial-contents
			  '((0 0 1) (0 1 0) (-1 0 0))))

(defconstant +z-rot+ (make-array
		      '(3 3) :initial-contents
		      '((0 1 0) (-1 0 0) (0 0 1))))

(defconstant +z-inv-rot+ (make-array
			  '(3 3) :initial-contents
			  '((0 -1 0) (1 0 0) (0 0 1))))

(defun generate-matrices ()
  (let ((ret nil))
    (let ((x-mat +identity+)
	  (x-inv +identity+))
      (loop for x from 0 to 3 do
	(let ((y-mat +identity+)
	      (y-inv +identity+))
	  (loop for y from 0 to 3 do
	    (let ((z-mat +identity+)
		  (z-inv +identity+))
	      (loop for z from 0 to 3 do
		(pushnew (cons (matrix-* x-mat (matrix-* y-mat z-mat))
			       (matrix-* z-inv (matrix-* y-inv x-inv)))
			 ret
			 :test #'equalp)
		(setf z-mat (matrix-* +z-rot+ z-mat)
		      z-inv (matrix-* z-inv +z-inv-rot+))))
	    (setf y-mat (matrix-* +y-rot+ y-mat)
		  y-inv (matrix-* y-inv +y-inv-rot+))))
	(setf x-mat (matrix-* +x-rot+ x-mat)
	      x-inv (matrix-* x-inv +x-inv-rot+))))

    (nreverse ret)))

(defun manhattan-distance (a b)
  (let ((a-x (aref a 0))
	(a-y (aref a 1))
	(a-z (aref a 2))

	(b-x (aref b 0))
	(b-y (aref b 1))
	(b-z (aref b 2)))
    (+ (abs (- a-x b-x))
       (abs (- a-y b-y))
       (abs (- a-z b-z)))))

(defun aoc-2021-day19 (input)
  (let* ((input-len (array-dimension input 0))
	 (completed-bitset #b1)
	 (full-bitset (1- (ash 1 input-len)))
	 (scanner-info (make-array input-len))
	 (beacons (hash-set:make-hash-set))
	 (found nil))

    ;; Populate `scanner-info' and populate it with the information of scanner 0
    ;; That is, its location
    (setf (aref scanner-info 0) #(0 0 0))

    ;; Populate `beacons' with the beacons of scanner 0
    (loop for beacon in (aref input 0) do
      (hash-set:hs-ninsert beacons (coerce beacon 'list)))

    (loop until (= completed-bitset full-bitset) do
      (loop for i from 0 below input-len
	    if (zerop (logand (ash completed-bitset (- i)) #b1))
	      do (let ((beacons-i (aref input i)))
		   (loop for (mat . inv) in (generate-matrices)
			 when (zerop (logand (ash completed-bitset (- i)) #b1)) do
			   (let ((transformed-beacons
				   (mapcar (lambda (v) (matrix-vec-* inv v)) beacons-i))
				 (offsets (make-hash-table :test 'equal)))
			     (loop for i-beacon in transformed-beacons do
			       (hash-set:dohashset (j-beacon beacons)
				 (incf
				  (gethash
				   (coerce (vector-- (coerce j-beacon 'vector) i-beacon) 'list)
				   offsets
				   0))))
			       (let ((coordinate nil))
				 (loop for (coord . occur) in (alexandria:hash-table-alist offsets) do
				   (when (>= occur 12)
				     (setf coordinate coord)
				     (loop-finish)))
				 (when coordinate
				   (setf coordinate (coerce coordinate 'vector))
				   (setf completed-bitset
					 (logior completed-bitset
						 (ash 1 i)))
				   (setf (aref scanner-info i) coordinate)
				   (loop for beacon in transformed-beacons do
				     (hash-set:hs-ninsert
				      beacons
				      (coerce (vector-+ beacon coordinate) 'list)))
				   (setf found t)
				   (loop-finish)))))
		   (setf found nil))))
    (values beacons scanner-info)))

(defun aoc-2021-day19-part1 (input)
  (hash-set:hs-count (nth-value 0 (aoc-2021-day19 input))))

(defun aoc-2021-day19-part2 (input)
  (let ((scanners (nth-value 1 (aoc-2021-day19 input))))
    (loop for vec-a across scanners
	  maximizing
	  (loop for vec-b across scanners
		maximizing
		(manhattan-distance vec-a vec-b)))))

(defconstant +input+ (parse-input (alexandria:read-file-into-string "day19-input")))

(format t "Part 1: ~A~%" (aoc-2021-day19-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day19-part2 +input+))
