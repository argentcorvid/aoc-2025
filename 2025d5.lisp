;;;2025 day 5

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  (add-package-local-nickname 'a 'alexandria-2))

(defparameter *day-number* 5)

(defparameter *test* nil)

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

(defun test-print (format-string &rest args)
  "print to standard out if *test* is t"
  (when *test*
    (apply #'format t format-string args)))

(define-modify-macro sortf (pred &rest args) sort)

(defun parse-input (string)
  (let ((out (make-hash-table)))
    (destructuring-bind (ranges-string available-string)
        (str:paragraphs string)
      (dolist (rng (str:split-omit-nulls #\newline ranges-string)
                   (sortf (the list (gethash :ranges out)) #'< :key #'first))
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
      (let ((found-range (find-if (lambda (range)
                                    (<= (the fixnum(first range))
                                        (the fixnum available-ingredient)
                                        (the fixnum(second range))))
                                  (the list (gethash :ranges in-table)))))
        (when found-range
          (test-print "~&fresh ingredient ~a found in range ~a" available-ingredient found-range)
          (incf fresh-count)))))) 

(defun p2 (in-table)
  (let* ((ranges (gethash :ranges in-table))
         (prev-start (the fixnum (first (first ranges))))
         (prev-end (the fixnum (second (first ranges))))
         (count (1+ (- prev-end prev-start))))
    (declare (type fixnum prev-start prev-end count))
    (test-print "~&first range:~a~% count: ~a "(pop ranges) count)
    (dolist (curr-range ranges count)
      (destructuring-bind (curr-start curr-end)
          curr-range
        (declare (type fixnum curr-start curr-end))
        (test-print "~&prev range: (~a ~a)~%current range: ~a" prev-start prev-end curr-range)
        (cond ((< prev-end curr-start)  ;no overlap, new range
               (setf prev-start curr-start
                     prev-end   curr-end)
               
               (test-print "~& no overlap. new range ~a~%  new count: ~a" curr-range (incf count (1+ (- curr-end curr-start)))))
              ((and (<= prev-start curr-start)
                    (<= curr-start prev-end))
               (test-print "~&ranges (~a ~a) and ~a overlap" prev-start prev-end curr-range)
               (if (< prev-end curr-end)
                   (progn (test-print " partially~& changing prev-end to ~a~% new count: ~a" curr-end (incf count (- curr-end prev-end)))
                          (setf prev-end curr-end))
                   (test-print " fully~& doing nothing"))))))))

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
  (let ((*test* t))
    (run parts (parse-input *test-input*))))
