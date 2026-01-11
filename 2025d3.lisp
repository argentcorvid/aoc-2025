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

(defun min-and-pos (battery-bank-in &key (start 0) (end (length battery-bank-in)))
    (declare (type fixnum start end)
                        (type vector battery-bank-in))
    (loop for position fixnum from start below end
                  for battery-joltage character = (aref battery-bank-in position)
                  with min-joltage character = #\A
                  with min-pos fixnum = end
                  when (char< battery-joltage min-joltage)
                              do (setf min-joltage battery-joltage
                                                          min-pos position)
                  finally (return (values min-joltage min-pos))))

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
  "throw out the (length - 12) lowest joltages"
  (labels ((rec (str start end)
             (if (<= (length str) number-to-keep)
                 str
                 (multiple-value-bind (ch pos)
                     (min-and-pos str :start start :end end)
                   (let ((new (str:concat (subseq str 0 pos)
                                          (subseq str (1+ pos)))))
                     (rec new (- (length new) number-to-keep) (1- (length new))))))))
    (parse-integer (rec battery-bank (- (length battery-bank) number-to-keep) (length battery-bank)))))

(defun p2 (battery-list)
  (let ((joltages (mapcar #'highest-override-joltage battery-list)))
    (when *verbose*
      (mapcar (lambda (in out)
                (format t "~&~a~%=> ~12d" in out))
              battery-list joltages ))
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
