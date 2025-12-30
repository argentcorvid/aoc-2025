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
  (loop :for (pt1 pt2)
          :on vertices
        :when (null pt2)
          :do (setf pt2 (first vertices))
        :collect (list pt1 pt2)))

(defun get-rectangle-edges (corner-pair)
  (if (apply #'some #'= corner-pair)
      corner-pair
      (destructuring-bind ((c1 r1)
                           (c2 r2))
          corner-pair
        (get-shape-edges (list (first corner-pair)
                               (list c1 r2)
                               (second corner-pair)
                               (list c2 r1))))))

(defun edge-length (edge)
  (assert  (apply #'some #'= edge) (edge) "points for edge ~a should have one coordinate on the same axis" edge)
  (destructuring-bind ((c1 r1)
                       (c2 r2))
      edge
    (max (abs (- c2 c1)) (abs (- r2 r1)))))

(defun edges-intersect-p (edge1 edge2)
  "check if edges intersect (but not on endpoints)"
  (destructuring-bind ((e1-c1 e1-r1)
                       (e1-c2 e1-r2))
      edge1
    (destructuring-bind ((e2-c1 e2-r1)
                         (e2-c2 e2-r2))
        edge2
      (let ((denom (- (* (- e1-c1 e1-c2)
                         (- e2-r1 e2-r2))
                      (* (- e1-r1 e1-r2)
                         (- e2-c1 e2-c2)))))
        (unless (zerop denom)
          (let ((v (/ (- (* (- e1-c1 e2-c1)
                            (- e2-r1 e2-r2))
                         (* (- e1-r1 e2-r1)
                            (- e2-c1 e2-c2)))
                      denom))
                (u (/ (- (- (* (- e1-c1 e1-c2)
                               (- e1-r1 e2-r1))
                            (* (- e1-r1 e1-r2)
                               (- e1-c1 e2-c1))))
                      denom)))
            (and (< 0 v 1)
                 (< 0 u 1))))))))

(defun point-on-edge-p (pt shape-edges)
  (flet ((pt-on-line-p (pt edge)
           (destructuring-bind ((col1 row1)
                                (col2 row2))
               edge
             (cond ((= row1 row2 (second pt))
                    (<= (min col1 col2) (first pt) (max col1 col2)))
                   ((= col1 col2 (first pt))
                    (<= (min row1 row2) (second pt) (max row1 row2)))))))
    (find pt shape-edges :test #'pt-on-line-p)))

(defun point-inside-shape-p (pt shape-edges)
  (let ((ray (list (list (first pt) 0) pt)))
    (oddp (count ray shape-edges :test #'edges-intersect-p))))

(defun valid-rectangle (rect-edges green-edges)
  )

(defun p2 (red-tiles)
  (let* ((green-edges (sort (get-shape-edges red-tiles) #'> :key #'edge-length)) ;longer edges are more likely to intersect
         (max-rect '(((0 0) (0 1)) 2)))
    (a:map-combinations (lambda (tile-pair)
                          (unless (apply #'some #'= tile-pair) ;single row or column rectangles arent going to be the largest
                            (when (valid-rectangle (get-rectangle-edges tile-pair)
                                                   green-edges)
                              (let ((area (rectangle-area tile-pair)))
                                (when (> area (second max-rect))
                                  (vformat "~&larger rectangle found: ~a, area: ~a" tile-pair area)
                                  (setf max-rect (list tile-pair area)))))))
                        red-tiles
                        :length 2)
    (apply #'vformat "~&Largest rectangle: ~a, area: ~a" max-rect)
    (second max-rect)))

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
