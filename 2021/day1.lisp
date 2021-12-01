(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)

(defun get-increasing-amount-part1 (vec)
  (let ((ret 0))
    (loop for x from 1 below (length vec)
	  when (< (aref vec (1- x)) (aref vec x))
	    do (incf ret))
    ret))

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

(defun get-increasing-amount-part2 (vec)
  (let ((window-vec (make-array 1 :adjustable t :fill-pointer 0)))
    (with-sliding-window (vec wnd :window-len 3)
      (vector-push-extend (reduce #'+ wnd :initial-value 0) window-vec))
    (get-increasing-amount-part1 window-vec)))

(defconstant +input+ (str:lines
		      (alexandria:read-file-into-string "day1-input")))


(defun aoc-2021-day1-part1 (input)
  (let ((vec (map 'vector #'parse-integer input)))
    (get-increasing-amount-part1 vec)))

(defun aoc-2021-day1-part2 (input)
  (let ((vec (map 'vector #'parse-integer input)))
    (get-increasing-amount-part2 vec)))

(format t "Part 1: ~D~%" (aoc-2021-day1-part1 +input+))
(format t "Part 2: ~D~%" (aoc-2021-day1-part2 +input+))
