;;;2025 day 2

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  (add-package-local-nickname 'a 'alexandria-2))

(defparameter *day-number* 2)
(defparameter *input-name-template* "2025d~dinput.txt")

(defparameter *test-input*
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defun parse-input (in-string)
  (let ((range-list-out))
    (dolist (range-string (str:split-omit-nulls #\, in-string) (nreverse range-list-out))
      (let ((sep-pos (position #\- range-string)))
        (push (list (parse-integer range-string :end sep-pos)
                    (parse-integer range-string :start (1+ sep-pos)))
              range-list-out)))))

(defun p1 (range-list)
  (let ((invalid-ids (list)))
    (dolist (curr-range range-list (reduce #'+ invalid-ids))
      (loop for id from (first curr-range) upto (second curr-range)
            when (equal-halves id)
              do (push id invalid-ids))))) 

(defun equal-halves (id)
  (let ((digits (ceiling (log id 10))))
    (when (and (plusp digits) ;id of 1 evaluates as zwro digits, and zwro is even.
               (evenp digits))
      (multiple-value-bind (left right) (floor id (expt 10 (/ digits 2)))
        (= left right)))))

(defun p2 ()
  )

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-string (uiop:read-file-string infile-name))
         (data (parse-input input-string)))
    (run parts data)))

(defun test (&rest parts)
  (run parts (parse-input *test-input*)))
