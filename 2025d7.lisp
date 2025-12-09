;;;2025 day 7

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  (add-package-local-nickname 'a 'alexandria-2))

(defparameter *day-number* 7)
(defparameter *input-name-template* "2025d~dinput.txt")

(defparameter *test-input*
  (str:lines ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............."))

(defparameter *verbose* nil)

(defun vformat (format-string &rest args)
  "print to standard output only if *verbose* is true"
  (when *verbose*
    (apply #'format t format-string args)))



(defun parse-input (lines)
  ;; (make-array (list (length lines) (length (first lines)))
  ;;              :initial-contents lines)
  ;; ?
  (let ((new (list)))
    (dolist (itm lines new)
      (setf new (append new (list itm))))))

(declaim (inline parse-input))

(defun dotp (char)
  (eql char #\.))

(defun p1 (tachyon-manifold)
  (let ((my-manifold (a:copy-sequence 'list tachyon-manifold))
        (split-count 0)
        (worlds (make-array (length (first tachyon-manifold)) :initial-element 0)))
    (setf (aref worlds (position #\S (first my-manifold))) 1)
    (do* ((curr-row-num 1 (incf curr-row-num))
          (prev-row (first my-manifold) curr-row)
          (curr-row (nth curr-row-num my-manifold) (nth curr-row-num my-manifold)))
         ((< (length my-manifold) curr-row-num) split-count)
      (dotimes (i (length curr-row))
        (unless (zerop (aref worlds i))
          (when (char= (aref curr-row i) #\^)
            (incf (aref worlds (1- i))) 
            (incf (aref worlds (1+ i)))
            (setf (aref worlds i) 0)
            (incf split-count)))))
    (dolist (line my-manifold)
      (fresh-line)
      (print line))
    (values split-count (reduce #'+ worlds)))) 

(defun run (parts-list data)
  (multiple-value-bind (p1-res p2-res)
      (p1 data)
    (dolist (part (a:ensure-list parts-list))
      (ccase part
        (1 (format t "~&Part 1: ~a" p1-res))
        (2 (format t "~&Part 2: ~a" p2-res))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (run parts data)))

(defun test (&rest parts)
  (let ((*verbose* t))
    (run parts (parse-input *test-input*))))
