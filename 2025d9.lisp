;;;2025 day 9

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  (add-package-local-nickname 'a 'alexandria-2))

(defparameter *day-number* 9)
(defparameter *input-name-template* "2025d~dinput.txt")

(defparameter *test-input*
  (str:lines "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")
  "col,row; largest rectangular area for p1: 50
p2:24")

(defparameter *verbose* nil)

(defun vformat (format-string &rest args)
  "print to standard output only if *verbose* is true"
  (when *verbose*
    (apply #'format t format-string args)))

(defun parse-input (lines)
  (let ((out (list)))
    (dolist (pt lines out)
      (push (mapcar #'parse-integer (str:split #\, pt))
            out))))

(defun rectangle-area (point-pair)
  (destructuring-bind ((col1 row1)
                       (col2 row2))
      point-pair
    (declare (fixnum  col1 col2 row1 row2))
    (* (1+ (abs (- col2 col1)))
       (1+ (abs (- row2 row1))))))

(defun p1 (red-tiles)
  (let ((max-rectangle 0))
    (declare (fixnum max-rectangle))
    (a:map-combinations (lambda (tile-pair)
                          (a:maxf max-rectangle (rectangle-area tile-pair)))
                        red-tiles
                        :length 2)
    max-rectangle))

(defun get-shape-edges (vertices)
  (flet ((make-line (c1 r1 c2 r2)
           (let* ((min-row (min r2 r1))
                  (min-col (min c2 c1))
                  (max-row (max r2 r1))
                  (max-col (max c2 c1))
                  (row-span (- max-row min-row))
                  (col-span (- max-col min-col)))
             (if (zerop row-span)
                 (mapcar #'list (a:iota col-span :start min-col)
                         (make-list col-span :initial-element max-row))
                 (mapcar #'list (make-list row-span :initial-element max-col)
                         (a:iota row-span :start min-row))
                 ))))
    (loop :for ((col1 row1)
                (col2 row2))
            :on vertices
          :by #'cdr
          :when (null col2)
            :do (setf col2 (first (first vertices))
                      row2 (second (first vertices)))
          :nconc (make-line col1 row1 col2 row2))))

(defun print-grid (in-list)
  (let* ((min-row 0)
         (min-col 0)
         (max-row (1+ (reduce #'max in-list :key #'second)))
         (max-col (1+ (reduce #'max in-list :key #'first)))
         (lines (loop :repeat max-row
                      :collect (make-string max-col :initial-element #\.))))
    (dolist (pt in-list)
      (destructuring-bind (c r) pt
        (setf (char (nth r lines) c) #\O)))
    (dolist (l lines)
      (fresh-line)
      (princ l))))

(defun p2 (red-tiles)
  (let* ((last-red (a:lastcar red-tiles))
         (shape-edges (get-shape-edges red-tiles)))
    )
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
