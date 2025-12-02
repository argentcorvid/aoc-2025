;;;2025 day 1

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  (add-package-local-nickname 'a 'alexandria-2))

(defparameter *day-number* 1)
(defparameter *input-name-template* "2025d~dinput.txt")

(defparameter *test-input*
  "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(defun parse-input (input-string)
  (mapcar (lambda (line)
            (ecase (schar line 0)
              (#\L (- (parse-integer line :start 1)))
              (#\R (parse-integer line :start 1))))
          (str:split-omit-nulls #\newline input-string)))

(defun p1 (movement-list)
  (loop with dial of-type fixnum = 50
        for steps of-type fixnum in movement-list
        do (setf dial (mod (+ dial steps) 100))
        counting (zerop dial))) 

(defun p2 (movement-list)
  (loop with dial of-type fixnum = 50
        for steps of-type fixnum in movement-list
        for (zeroes new-dial) = (multiple-value-list (floor (+ steps dial) 100))
          do (setf dial new-dial)
        summing (abs zeroes)))

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-string infile-name))
         (data (parse-input input-lines)))
    (run parts data)))

(defun test (&rest parts)
  (run parts (parse-input *test-input*)))
