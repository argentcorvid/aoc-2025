;;;2025 day 8

(eval-when (:compile-toplevel :cload-toplevel)
  (ql:quickload '(:alexandria :str))
  (add-package-local-nickname 'a 'alexandria-2))

(defparameter *day-number* 8)
(defparameter *input-name-template* "2025d~dinput.txt")

(defparameter *test-input*
  (str:lines
   "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")
  "p1: after doing 10 connections, 3 largest circuits are 5,4,2; multiple 40")


(defparameter *verbose* nil)

(defun vformat (format-string &rest args)
  "print to standard output only if *verbose* is true"
  (when *verbose*
    (apply #'format t format-string args)))

(defun point-distance (p1 p2)
  (isqrt (loop for c1 fixnum in p1
               and c2 fixnum in p2
               sum (expt (- c2 c1) 2))))

(defun pair-distance (point-pair)
  (apply #'point-distance point-pair))

(define-modify-macro sortf (pred &rest args) sort)

(defun parse-input (lines)
  (mapcar (lambda (line)
            (mapcar #'parse-integer (str:split #\, line)))
          lines))

(defun p1 (j-boxes-in num-to-connect &optional ( number-of-circuits 3))
  (let ((pairs (list))
        (circuits (mapcar #'list j-boxes-in)))
    (a:map-combinations (lambda (point-pair)
                          (push point-pair pairs))
                        j-boxes-in
                        :length 2))
  (sortf pairs #'< :key #'pair-distance)
  (setf pairs (subseq pairs 0 (1- num-to-connect)))
  (mapc (lambda (pair)
          (let ((it (find pair circuits :test (lambda (a b)
                                                (intersection a b :test #'equal)))))
            (when it
              (a:unionf it pair :test #'equal))) )
        pairs)
  (reduce #'* circuits :key #'length :end 2)) ;need to sort by size

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
