;;;2025 day 4

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  (add-package-local-nickname 'a 'alexandria-2))

(defparameter *day-number* 4)
(defparameter *input-name-template* "2025d~dinput.txt")

(defparameter *test-input*

  '#.(str:split-omit-nulls #\newline
                        "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")
    "given result is 13")

(defparameter *look8*
  '#.(loop for col from -1 to 1
           nconc (loop for row from -1 to 1
                       unless (= 0 row col)
                         collect (list col row))))

(defun parse-input (lines)
  (make-array (list (length (first lines)) (length lines)) :initial-contents lines))

(defun p1 (paper-grid)
  (loop with (cols rows) = (array-dimensions paper-grid)
        for row from 0 below rows
        summing (loop for col from 0 below cols
                      counting (reachable paper-grid col row))))

(defun reachable (grid col row)
  (loop with pos = (list col row)
        and count = 0
        for look-delta in *look8*
        for (look-col look-row) = (mapcar #'+ pos look-delta)
        when (>= count 4)
          return nil
        when (and (array-in-bounds-p grid look-col look-row)
                  (equal #\@ (aref grid look-col look-row)))
          do (incf count)
                                        ;even need the comapre here?
        finally (return (when (< count 4)
                          (format t "~&reachable: ~a,~a" col row)
                          t)))) 

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
  (run parts (parse-input *test-input*)))
