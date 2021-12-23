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

(defconstant +input+
  (iterate:iter (iterate:for c in-string
			     (alexandria:read-file-into-string "day23-input"))
    (when (or (char= c #\.)
	      (char<= #\A c #\D))
      (iterate:collect c result-type string))))

(defconstant +hallway-size+
  (iterate:iter (iterate:for c in-string +input+)
    (iterate:counting (char= c #\.))))

(defstruct game-state
  world
  energy)

(defun room-organized-p (world depth r)
  (iterate:iter (iterate:for i from (1- depth) downto 0)
    (let ((c (aref world (+ +hallway-size+ (* 4 i) r))))
      (when (char= c #\.)
	(return-from room-organized-p t))
      (when (char/= c (code-char (+ (char-code #\A) r)))
	(return-from room-organized-p nil))))
  t)

(defun room-count (world depth r)
  (let ((count 0))
    (iterate:iter (iterate:for i from (1- depth) downto 0)
      (let ((c (aref world (+ +hallway-size+ (* 4 i) r))))
	(when (char= c #\.)
	  (return-from room-count count))
	(incf count)))
    count))

(defun push-room (world depth r c)
  (iterate:iter (iterate:for i from (1- depth) downto 0)
    (let ((index (+ +hallway-size+ (* 4 i) r)))
      (when (char= (aref world index) #\.)
	(setf (aref world index) c)
	(return-from push-room))))
  (error 'cannot-push-room))

(defun pop-room (world depth r)
  (iterate:iter (iterate:for i from 0 below depth)
    (let ((index (+ +hallway-size+ (* 4 i) r)))
      (when (char/= (aref world index) #\.)
	(setf (aref world index) #\.)
	(return-from pop-room))))
  (error 'cannot-pop-room))

(defun peek-room (world depth r)
  (iterate:iter (iterate:for i from 0 below depth)
    (let* ((index (+ +hallway-size+ (* 4 i) r))
	   (c (aref world index)))
      (when (char/= c #\.)
	(return-from peek-room c))))
  (error 'cannot-peek-room))

(defconstant +cost+ (make-array 4 :initial-contents '(1 10 100 1000)))
(defconstant +directions+ (make-array 2 :initial-contents '(-1 1)))

(defun get-neighbours (depth state)
  (with-slots (world energy) state
    (let ((neighbours (make-array 1 :adjustable t :fill-pointer 0)))
      ;; 1st: move an amphipod from a hallway to their target room
      (iterate:iter (iterate:for i from 0 below +hallway-size+)
	(when (char= (aref world i) #\.)
	  (iterate:next-iteration))

	(let* ((picked (aref world i))
	       (picked-index (- (char-code picked) (char-code #\A)))
	       (can-move-to-room (room-organized-p world depth picked-index)))
	  (unless can-move-to-room
	    (iterate:next-iteration))

	  (let* ((target-position (+ 2 (* 2 picked-index)))
		 (direction (if (> target-position i) 1 -1)))

	    (let ((j direction)
		  (should-iterate t))
	      (loop while (and should-iterate
			       (<= (abs j) (abs (- target-position i))))
		    do (when (char/= (aref world (+ i j)) #\.)
			 (setf can-move-to-room nil)
			 (setf should-iterate nil))
		       (incf j direction)))

	    (unless can-move-to-room
	      (iterate:next-iteration))

	    (let ((new-world (copy-seq world)))
	      (setf (aref new-world i) #\.)
	      (push-room new-world depth picked-index picked)

	      (let ((new-energy (+ energy
				   (* (+ (abs (- target-position i))
					 (- depth
					    (room-count world depth picked-index)))
				      (aref +cost+ picked-index)))))
		(vector-push-extend (make-game-state :world new-world
						     :energy new-energy)
				    neighbours))))))

      ;; Don't remove amphipods from rooms if the rooms are ready to be filled
      (when (> (length neighbours) 0)
	(return-from get-neighbours neighbours))

      ;; 2nd: Remove one amphipod from a room
      (iterate:iter (iterate:for r from 0 below 4)
	;; If the room is organizer, we cannot take an amphipod from this one
	(when (room-organized-p world depth r)
	  (iterate:next-iteration))

	(let* ((picked (peek-room world depth r))
	       (picked-index (- (char-code picked) (char-code #\A)))
	       ;; Possible targets are empty spaces on hallway, not in front of any
	       ;; room, with no blockers
	       (energy (+ energy
			  (* (1+ (- depth
				    (room-count world depth r)))
			     (aref +cost+ picked-index))))
	       (room-position (+ 2 (* 2 r))))
	  (iterate:iter (iterate:for direction in-vector +directions+)
	    (let ((distance direction))
	      (iterate:iter
		(iterate:while (and (>= (+ room-position distance) 0)
				    (< (+ room-position distance) +hallway-size+)
				    (char= (aref world (+ room-position distance))
					   #\.)))
		(when (member (+ room-position distance)
			      '(2 4 6 8)
			      :test #'=)
		  ;; We're in front of a room, let's skip this
		  (incf distance direction)
		  (iterate:next-iteration))

		(let ((new-world (copy-seq world)))
		  (setf (aref new-world (+ room-position distance)) picked)
		  (pop-room new-world depth r)
		  (vector-push-extend
		   (make-game-state :world new-world
				    :energy (+ energy
					       (* (abs distance)
						  (aref +cost+ picked-index))))
		   neighbours))
		(incf distance direction))))))
      neighbours)))

(defun target-p (state depth)
  (with-slots (world) state
    (iterate:iter (iterate:for i from 0 below +hallway-size+)
      (when (char/= (aref world i) #\.)
	(return-from target-p nil)))
    (iterate:iter (iterate:for r from 0 below 4)
      (unless (room-organized-p world depth r)
	(return-from target-p nil)))
    t))

(defun organize (initial-state)
  ;; Dijkstra on the graph
  (let ((depth (/ (- (length (game-state-world initial-state)) +hallway-size+) 4))
	(frontier (priority-queue:make-pqueue #'<))
	(visited (hash-set:make-hash-set)))
    (priority-queue:pqueue-push initial-state 0 frontier)

    (iterate:iter (iterate:until (priority-queue:pqueue-empty-p frontier))
      (let* ((node (priority-queue:pqueue-pop frontier))
	     (world-string (copy-seq (game-state-world node))))
	(when (hash-set:hs-memberp visited world-string)
	  (iterate:next-iteration))
	(when (target-p node depth)
	  (return-from organize (game-state-energy node)))
	(hash-set:hs-ninsert visited world-string)
	(iterate:iter (iterate:for n in-vector (get-neighbours depth node))
	  (priority-queue:pqueue-push n (game-state-energy n) frontier)))))
  (error 'organize-impossible))

(defun aoc-2021-day23-part1 (input)
  (organize (make-game-state :world input :energy 0)))

(defconstant +world-extension-chars+
  (iterate:iter (iterate:for c in-string "  #D#C#B#A#
  #D#B#A#C#")
    (when (or (char= c #\.)
	      (char<= #\A c #\D))
      (iterate:collect c result-type string))))

(defun aoc-2021-day23-part2 (input)
  (let ((deep-world (format nil "~{~A~}"
			    `(,(subseq input 0 (+ +hallway-size+ 4))
			      ,+world-extension-chars+
			      ,(subseq input (+ +hallway-size+ 4))))))
    (organize (make-game-state :world deep-world :energy 0))))

(format t "Part 1: ~A~%" (aoc-2021-day23-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day23-part2 +input+))
