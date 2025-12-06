;;;2025 day 5

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  (add-package-local-nickname 'a 'alexandria-2))

(defparameter *day-number* 5)
(defparameter *input-name-template* "2025d~dinput.txt")

(defparameter *test-input*
  "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

(define-modify-macro sortf (pred &rest args) sort)

(defun parse-input (string)
  (let ((out (make-hash-table)))
    (destructuring-bind (ranges-string available-string)
        (str:paragraphs string)
      (dolist (rng (str:split-omit-nulls #\newline ranges-string)
                   (sortf (gethash :ranges out) #'< :key #'first))
        (push (mapcar #'parse-integer (str:split #\- rng)) (gethash :ranges out (list))))
      (setf (gethash :available out)
            (mapcar #'parse-integer (str:split-omit-nulls #\newline available-string)))
      out)))

(defun p1 (in-table)
  (let ((fresh-count 0))
    (declare (fixnum fresh-count))
    (dolist (available-ingredient
             (gethash :available in-table)
             fresh-count)
      (when (find-if (lambda (range)
                       (<= (first range)
                           available-ingredient
                           (second range)))
                     (gethash :ranges in-table))
        ;(print available-ingredient)
        (incf fresh-count))))) 

(defun p2 ()
  )

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input (uiop:read-file-string infile-name))
         (data (parse-input input)))
    (run parts data)))

(defun test (&rest parts)
  (run parts (parse-input *test-input*)))
