(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)

(defclass bingo-board ()
  ((numbers :initarg :numbers
	    :accessor bingo-board-numbers)
   (marked :initarg :marked
	   :accessor bingo-board-marked)
   (done :initform nil
	 :accessor bingo-board-done)))

(defmethod print-object ((object bingo-board) stream)
  (with-slots (numbers marked done) object
    (print-unreadable-object (object stream :type t)
      (format stream ":NUMBERS ~s :MARKED ~s :DONE ~s" numbers marked done))))

(defclass bingo-state ()
  ((numbers :initarg :numbers
	    :accessor bingo-state-numbers)
   (boards :initarg :boards
	   :accessor bingo-state-boards)))

(defmethod print-object ((object bingo-state) stream)
  (with-slots (numbers boards) object
    (print-unreadable-object (object stream :type t)
      (format stream ":NUMBERS ~s :BOARDS ~A" numbers boards))))

(defun parse-bingo-board (lines)
  (let ((numbers (make-array '(5 5)))
	(marked (make-array '(5 5) :initial-element nil)))
    (dotimes (y 5)
      (let* ((line (str:split-omit-nulls " " (car lines)))
	     (nums (mapcar #'parse-integer line)))
	(dotimes (x 5)
	  (setf (aref numbers y x) (car nums))
	  (setf nums (cdr nums))))
      (setf lines (cdr lines)))
    (make-instance 'bingo-board :marked marked :numbers numbers)))

(defun parse-bingo-state (lines)
  (let ((boards (make-array 1 :adjustable t :fill-pointer 0))
	(numbers (mapcar #'parse-integer (str:split "," (car lines)))))
    (setf lines (cdr lines))
    (loop while (and lines (string-equal "" (car lines)))
	  do (vector-push-extend (parse-bingo-board (cdr lines)) boards)
	     (dotimes (y 6) (setf lines (cdr lines))))
    (make-instance 'bingo-state :numbers numbers
				:boards boards)))

(defun mark-found (board number)
  (with-slots (numbers marked) board
    (loop for x from 0 below 5
	  do (loop for y from 0 below 5
		   do (when (= (aref numbers y x) number)
			(setf (aref marked y x) t))))))

(defun bingo-board-winner-p (board)
  (with-slots (marked) board
    (dotimes (y 5)
      (let ((row nil))
	(dotimes (x 5)
	  (push (aref marked y x) row))
	(when (every #'identity row)
	  (return-from bingo-board-winner-p t))))
    (dotimes (x 5)
      (let ((row nil))
	(dotimes (y 5)
	  (push (aref marked y x) row))
	(when (every #'identity row)
	  (return-from bingo-board-winner-p t)))))
  nil)

(defun get-unmarked-sum (board)
  (let ((sum 0))
    (with-slots (numbers marked) board
      (dotimes (y 5)
	(dotimes (x 5)
	  (unless (aref marked y x)
	    (setf sum (+ sum (aref numbers y x)))))))
    sum))

(defun find-best-match-part1 (state)
  (let ((boards (bingo-state-boards state)))
    (loop for num in (bingo-state-numbers state)
	  do (loop for board across boards
		   do (mark-found board num))
	     (loop for board across boards
		   if (bingo-board-winner-p board)
		     do (return-from find-best-match-part1
			  (* num (get-unmarked-sum board)))))))

(defun find-best-match-part2 (state)
  (let ((boards (bingo-state-boards state))
	(victories nil))
    (loop for num in (bingo-state-numbers state)
	  do (loop for board across boards
		   do (mark-found board num))
	     (loop for board across boards
		   if (and (not (bingo-board-done board))
			   (bingo-board-winner-p board))
		     do (setf (bingo-board-done board) t)
			(push (* num (get-unmarked-sum board)) victories)))
    (car victories)))

(defconstant +input+ (str:lines (alexandria:read-file-into-string "day4-input")))

(defun aoc-2021-day4-part1 (input)
  (let ((state (parse-bingo-state input)))
    (find-best-match-part1 state)))

(defun aoc-2021-day4-part2 (input)
  (let ((state (parse-bingo-state input)))
    (find-best-match-part2 state)))

(format t "Part 1: ~A~%" (aoc-2021-day4-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day4-part2 +input+))
