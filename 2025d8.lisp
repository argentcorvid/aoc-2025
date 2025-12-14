;;;2025 day 8

(eval-when (:compile-toplevel :load-toplevel)
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

(declaim (inline equal-intersection
                                  equal-union
                                  pair-distance
                                  point-distance))

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

(defun equal-intersection (list1 list2 &rest args)
  "intersection using #'equal"
  (apply #'intersection list1 list2 :test #'equal args))

(defun equal-union (list1 list2 &rest args)
  (apply #'union list1 list2 :test #'equal args))



(defun join-circuits (c1 c2 circuits-in)
  (let ((new-circuit (equal-union c1 c2)))
    (cons new-circuit (remove new-circuit circuits-in :test #'equal-intersection))))

(defun p1 (j-boxes-in num-to-connect &optional ( number-of-circuits 3))
  (let ((pairs (list))
        (circuits (mapcar #'list j-boxes-in)))
    (a:map-combinations (lambda (point-pair)
                          (push point-pair pairs))
                        j-boxes-in
                        :length 2)
    (sortf pairs #'< :key #'pair-distance)
    (vformat "~&10 closest pairs:~& ~a" (subseq pairs 0 10))
    (mapc (lambda (pair)
            (destructuring-bind (c1 c2)
                (loop :for point :in pair
                      :collect (find (list point) circuits
                                     :test #'equal-intersection))
              
              (if (or c1 c2)
                  (progn (vformat "~&found ~@[~a  ~]~@[~a ~]in circuits~% for pair ~a" c1 c2 pair)
                         (setf circuits (join-circuits  c1 c2 circuits)))  
                  (progn (vformat "~&no match for pair ~a" pair))))
            (vformat "~&  circuits now: ~a" circuits))
          (subseq pairs 0 num-to-connect))
  
    (reduce #'* (sort (mapcar #'length circuits) #'>)
            :end number-of-circuits :initial-value 1))) 

(defun p2 ()
  )

(defun run (parts-list data)
  )

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (dolist (part (a:ensure-list parts))
      (ccase part
        (1 (format t "~&Part 1: ~a" (p1 data 1000)))
        (2 (format t "~&Part 2: ~a" (p2 data)))))))

(defun test (&rest parts)
  (let ((*verbose* t)
        (data (parse-input *test-input*)))
    (dolist (part (a:ensure-list parts))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data 10)))
      (2 (format t "~&Part 2: ~a" (p2 data)))))))
