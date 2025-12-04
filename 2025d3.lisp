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

(defun max-and-pos (battery-bank-in &key (start 0) (end (length string-in)))
  (loop for position from start below end
        for battery-joltage = (schar battery-bank-in position)
        with max-joltage = #\0
        with max-pos = 0
        when (char< max-joltage battery-joltage)
          do (setf max-joltage battery-joltage
                   max-pos position)
        finally (return (values max-joltage max-pos))))

(defun highest-bank-joltage (battery-bank)
  (if (str:empty? battery-bank)
      0)
  (multiple-value-bind (highest-joltage highest-position)
     (max-and-pos battery-bank :end (1- (length battery-bank)))
    (multiple-value-bind (2nd-highest-joltage 2nd-highest-pos)
        (max-and-pos battery-bank :start (1+ highest-position))
      (let ((best ()))
        (format t "bank: ~a, best joltage: ~a")))))

(defun p2 ()
  )

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
  (run parts *test-inpu*))
