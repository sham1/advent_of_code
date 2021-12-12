(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)

(defun parse-input (input)
  (let ((entries (make-hash-table :test 'equal)))
    (loop for line in (str:lines input) do
      (let* ((split (str:split "-" line))
	     (start (car split))
	     (end (cadr split))
	     (start-neighbors (gethash start entries nil))
	     (end-neighbors (gethash end entries nil)))
        (setf (gethash start entries)
	      (union `(,end) start-neighbors :test #'string=))
	(setf (gethash end entries)
	      (union `(,start) end-neighbors :test #'string=))))
    entries))

(defun small-cave-p (name)
  (char<= #\a (aref name 0) #\z))

(defun generate-neighbour-paths (path neighbours)
  (loop for neighbour in neighbours
	collecting (cons neighbour path)))

(defun part1-valid-path-p (path)
  (let ((hash (make-hash-table :test 'equal)))
    (loop for cave in path do
      (when (small-cave-p cave)
	(incf (gethash cave hash 0))))
    (not (some (lambda (n) (>= n 2)) (alexandria:hash-table-values hash)))))

(defun part2-valid-path-p (path)
  (let ((hash (make-hash-table :test 'equal))
	(more-than-once nil))
    (loop for cave in path do
      (when (small-cave-p cave)
	(incf (gethash cave hash 0))))
    (loop for (cave . visits) in (alexandria:hash-table-alist hash)
	  when (and (member cave '("start" "end") :test #'string=)
		    (>= visits 2))
	    do (return-from part2-valid-path-p nil)
	  when (> visits 2)
	    do (return-from part2-valid-path-p nil)
	  when (= visits 2)
	    do (push visits more-than-once))
    (not (> (length more-than-once) 1))))

(defun get-paths-part1 (input)
  (let ((completed-paths nil)
	(queue (list '("start"))))
    (loop while (not (null queue)) do
      (let* ((path (car queue))
	     (neighbours (gethash (car path) input)))
	(setf queue (cdr queue))
	(loop for path in (generate-neighbour-paths path neighbours) do
	  (when (part1-valid-path-p path)
	    (if (string= (car path) "end")
		;; Have we reached the end?
		;; If so, push the path to the results
		(push path completed-paths)
		;; Otherwise, push the new path to the queue
		(push path queue))))))
    completed-paths))

(defun get-paths-part2 (input)
  (let ((completed-paths nil)
	(queue (list '("start"))))
    (loop while (not (null queue)) do
      (let* ((path (car queue))
	     (neighbours (gethash (car path) input)))
	(setf queue (cdr queue))
	(loop for path in (generate-neighbour-paths path neighbours) do
	  (when (part2-valid-path-p path)
	    (if (string= (car path) "end")
		;; Have we reached the end?
		;; If so, we should know this to be a valid path
		(push path completed-paths)
		;; Otherwise, push the new path to the queue
		(push path queue))))))
    completed-paths))

(defun aoc-2021-day12-part1 (input)
  (length (get-paths-part1 input)))

(defun aoc-2021-day12-part2 (input)
  (length (get-paths-part2 input)))

(defconstant +input+ (parse-input (alexandria:read-file-into-string "day12-input")))

(format t "Part 1: ~A~%" (aoc-2021-day12-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day12-part2 +input+))
