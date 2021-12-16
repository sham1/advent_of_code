(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :iterate)
(ql:quickload :priority-queue)

(defclass bits-packet ()
  ((version :initarg :version
	    :accessor bits-packet-version)
   (type :initarg :type
	 :accessor bits-packet-type)
   (data :initarg :data
	 :accessor bits-packet-data)))

(defmethod print-object ((object bits-packet) stream)
  (with-slots (version type data) object
    (print-unreadable-object (object stream :type t)
      (format stream ":VERSION ~A :TYPE ~A :DATA ~s"
	      version type data))))

(defun parse-hex-char (char)
  (cond
    ((char<= #\0 char #\9) (- (char-code char) (char-code #\0)))
    ((char<= #\A char #\F) (+ (- (char-code char) (char-code #\A)) 10))))

(defun read-bit (bytes bit-position)
  (let ((byte-position (floor bit-position 8))
	(subbyte-position (mod bit-position 8)))
    (logand (ash (aref bytes byte-position) (- subbyte-position 7)) #b1)))

(defun bit-slice (bytes bit-position &optional (bit-count 1))
  (let ((ret 0))
    (loop for bit-pos from bit-position below (+ bit-position bit-count) do
      (setf ret (logior (ash ret 1) (read-bit bytes bit-pos))))
    ret))

(defun round-to-multiple (num boundary)
  (let ((modulo (mod num boundary)))
    (if (zerop modulo)
	num
	(+ num (- boundary modulo)))))

(defun parse-packet (bytes &optional (bit-position 0))
  (let ((version (bit-slice bytes bit-position 3))
	(packet-type (bit-slice bytes (+ bit-position 3) 3))
	(packet nil)
	(end-bit 0))
    (if (= packet-type 4)
	(let ((literal-value 0))
	  (loop for bit-pos from (+ bit-position 6) by 5 do
	    (let ((continue-bit (read-bit bytes bit-pos))
		  (slice (bit-slice bytes (1+ bit-pos) 4)))
	      (setf literal-value (logior (ash literal-value 4)
					  slice))
	      (setf end-bit (+ bit-pos 5))
	      (when (= continue-bit 0)
		(return))))
	  (setf packet
		(make-instance 'bits-packet :data literal-value
					     :type 'literal
					     :version version)))
	(let ((length-type-id (read-bit bytes (+ bit-position 6))))
	  (if (= length-type-id 0)
	      (let* ((packets-length (bit-slice bytes (+ bit-position 7) 15))
		     (position (+ bit-position 22))
		     (end-pos (+ position packets-length))
		     (packets nil))
		(loop while (< position end-pos) do
		  (multiple-value-bind (p end)
		      (parse-packet bytes position)
		    (setf position end)
		    (push p packets)))
		(setf end-bit position)
		(setf packet (make-instance 'bits-packet :data (nreverse packets)
							  :type packet-type
							  :version version)))
	      (let ((number-of-subpackets (bit-slice bytes (+ bit-position 7) 11))
		    (position (+ bit-position 18))
		    (packets nil))
		(dotimes (i number-of-subpackets)
		  (multiple-value-bind (p end)
		      (parse-packet bytes position)
		    (setf position end)
		    (push p packets)))
		(setf end-bit position)
		(setf packet (make-instance 'bits-packet :data (nreverse packets)
							  :type packet-type
							  :version version))))))
    (unless (eql (bits-packet-type packet) 'literal)
      (let ((packet-type (bits-packet-type packet)))
	(setf
	 (bits-packet-type packet)
	 (cond
	   ((= packet-type 0) 'sum)
	   ((= packet-type 1) 'product)
	   ((= packet-type 2) 'minimum)
	   ((= packet-type 3) 'maximum)
	   ((= packet-type 5) 'greater)
	   ((= packet-type 6) 'less)
	   ((= packet-type 7) 'equal)
	   (t (error "Unknown packet type"))))))
    (values packet end-bit)))

(defun eval-packet (packet)
  (with-slots (type data) packet
    (ecase type
      (literal data)
      (sum (reduce (lambda (acc p) (+ acc (eval-packet p))) data :initial-value 0))
      (product (reduce (lambda (acc p) (* acc (eval-packet p))) data :initial-value 1))
      (minimum (reduce (lambda (acc p) (min acc (eval-packet p))) data :initial-value most-positive-fixnum))
      (maximum (reduce (lambda (acc p) (max acc (eval-packet p))) data :initial-value most-negative-fixnum))
      (greater (destructuring-bind (one two) data
		 (if (> (eval-packet one) (eval-packet two))
		     1
		     0)))
      (less (destructuring-bind (one two) data
	      (if (< (eval-packet one) (eval-packet two))
		  1
		  0)))
      (equal (destructuring-bind (one two) data
	       (if (= (eval-packet one) (eval-packet two))
		   1
		   0))))))

(defun parse-input (input)
  (let* ((input (car (str:lines input)))
	 (bytes (make-array (ceiling (length input) 2) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length input) by 2 do
      (let ((num (logior (ash (parse-hex-char (aref input i)) 4)
			 (parse-hex-char (aref input (1+ i))))))
	(setf (aref bytes (floor i 2)) (logior num))))
    (parse-packet bytes)))

(defconstant +input+
  (parse-input (alexandria:read-file-into-string "day16-input")))

(defun aoc-2021-day16-part1 (input)
  (let ((ret 0)
	(stack (list input)))
    (loop until (null stack) do
      (let ((packet (car stack)))
	(setf stack (cdr stack))
	(incf ret (bits-packet-version packet))
	(unless (eql 'literal (bits-packet-type packet))
	  (loop for child in (bits-packet-data packet) do
		(push child stack)))))
    ret))

(defun aoc-2021-day16-part2 (input)
  (eval-packet input))

(format t "Part 1: ~A~%" (aoc-2021-day16-part1 +input+))
(format t "Part 2: ~A~%" (aoc-2021-day16-part2 +input+))
