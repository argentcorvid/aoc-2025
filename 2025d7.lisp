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
  lines
  )

(declaim (inline parse-input))

(defun dotp (char)
  (eql char #\.))

(defun p1 (tachyon-manifold)
  (let ((my-manifold (a:copy-sequence 'list tachyon-manifold))
        (split-count 0))
    (setf (aref (first my-manifold) (position #\S (first my-manifold))) #\|)
    (do* ((curr-row-num 1 (incf curr-row-num))
          (prev-row (first my-manifold) curr-row)
          (curr-row (nth curr-row-num my-manifold) (nth curr-row-num my-manifold)))
         ((< (length my-manifold) curr-row-num) split-count)
      (dotimes (i (length curr-row))
        (when (char= #\| (aref prev-row i))
          (case (aref curr-row i)
            (#\. (setf (aref curr-row i) #\|))
            (#\^ (setf (aref curr-row (1- i)) #\|
                     (aref curr-row (1+ i)) #\|)
             (incf split-count)))))
      endloop)
    (dolist (line my-manifold)
      (fresh-line)
      (print line))
    split-count)) 

(defun p2 ()
  )

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (run parts data)))

(defun test (&rest parts)
  (let ((*verbose* t))
    (run parts (parse-input *test-input*))))
