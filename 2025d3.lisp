;;;2025 day 3

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  (add-package-local-nickname 'a 'alexandria-2))

(defparameter *day-number* 3)
(defparameter *input-name-template* "2025d~dinput.txt")

(defparameter *test-input*
  (list "987654321111111"
        "811111111111119"
        "234234234234278"
        "818181911112111"))

(defun p1 (battery-list)
  (reduce #'+ (mapcar #'highest-bank-joltage battery-list))) 

(defun max-and-pos (battery-bank-in &key (start 0) (end (length battery-bank-in)))
  (declare (type fixnum start end)
           (type vector battery-bank-in))
  (loop for position fixnum from start below end
        for battery-joltage character = (aref battery-bank-in position)
        with max-joltage character = #\0
        with max-pos fixnum = 0
        when (char< max-joltage battery-joltage)
          do (setf max-joltage battery-joltage
                   max-pos position)
        finally (return (values max-joltage max-pos))))

(defun highest-bank-joltage (battery-bank)
  (if (str:empty? battery-bank)
      0
      (multiple-value-bind (highest-joltage highest-position)
          (max-and-pos battery-bank :end (1- (length battery-bank)))
        (multiple-value-bind (2nd-highest-joltage 2nd-highest-pos)
            (max-and-pos battery-bank :start (1+ highest-position))
          (let ((best (concatenate 'string (list highest-joltage 2nd-highest-joltage))))
            (format t "~&bank: ~a, best joltage: ~a~&" battery-bank best)
            (parse-integer best))))))

(defun highest-override-joltage (battery-bank &key (number-to-keep 12))
  "throw out the (length - number-to-keep) lowest joltages

-extend logic/generalize from highest-bank-joltage
-- 1. find max digit in bank from start to (- (length bank) number-to-keep -1)
-- 2. collect/concat character
-- 3. reucrse, using (+ pos 1) as start and (+ end 1) as end, stop when length = number-to-keep"
  (declare (simple-string battery-bank)
           (fixnum number-to-keep))
  (labels ((rec (str start end &optional (accum (make-string 0)))
             (declare (dynamic-extent str start end)
                      (fixnum start end))
             (if (= number-to-keep (length accum))
                 accum
                 (multiple-value-bind (ch pos)
                     (max-and-pos str :start start :end end)
                   (rec str (1+ pos) (1+ end) (str:concat accum (string ch)))))))
    (parse-integer (rec battery-bank 0 (- (length battery-bank) number-to-keep -1)))))

(defun p2 (battery-list)
  (let ((joltages (mapcar #'highest-override-joltage battery-list)))
    (when *verbose*
      (mapcar (lambda (in out)
                (format t "~&~a~%=> ~12d" in out))
              battery-list joltages )
      (fresh-line))
    (reduce #'+ joltages)))

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name)))
    (run parts input-lines)))

(defun test (&rest parts)
  (run parts *test-input*))
