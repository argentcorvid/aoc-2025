;;;2025 day 1

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:iterate :alexandria :str :series :for))
  (add-package-local-nickname 'a 'alexandria-2)
  (add-package-local-nickname 'i 'iterate))

(defparameter *day-number* 1)
(defparameter *input-name-template* "2025d~dinput.txt")

(defparameter *test-input*
  "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(defun parse-input (input-string)
  (mapcar (lambda (line)
            (ecase (schar line 0)
              (#\L (- (parse-integer line :start 1)))
              (#\R (parse-integer line :start 1))))
          (str:lines input-string)))

(defun p1 (movement-list)
  (loop with dial of-type fixnum = 50
        for steps of-type fixnum in movement-list
        do (setf dial (mod (+ dial steps) 100))
        counting (zerop dial)))

(defun series-parse (input)
  (series:mapping ((line (series:scan (str:lines input))))
                  (if (char= #\R (char line 0))
                      (the fixnum (parse-integer (subseq line 1)))
                      (the fixnum (- (parse-integer (subseq line 1)))))))

(defun series-p1 (movement-series)
  (let ((dial 50))
    (declare (fixnum dial)
             (series::basic-series movement-series))
    (series:collect-length
     (series:choose-if #'identity
                       (series:mapping ((steps movement-series))
                                       (zerop (setq dial (mod (+ dial steps) 100))))))))

(defun iter-p2 (movement-list)
  (i:iterate
    (i:for dial initially 50 then new-dial)
    (i:for raw-steps in movement-list)
    (i:for (values times-through-100 steps) = (truncate raw-steps 100))
    (i:for new-dial = (+ dial steps))
    (declare (fixnum dial raw-steps times-through-100 steps new-dial))
    (setf times-through-100 (abs times-through-100))
    (when (and (plusp dial)
               (not (< 0 new-dial 100)))
      (incf times-through-100))
    (setf new-dial (mod new-dial 100))
    (i:sum times-through-100)))

(defun p2 (movement-list)
  (format t "~&dial: 50,")
  (loop :for dial fixnum := 50 :then new-dial
        :for raw-steps fixnum :in movement-list
        :for (times-through-100 steps) (fixnum fixnum) := (multiple-value-list (truncate raw-steps 100))
        :for new-dial := (+ dial steps)
        :do (setf times-through-100 (abs times-through-100))
            (when (and (plusp dial)
                       (not (< 0 new-dial 100)))
              (incf times-through-100))
            (setf new-dial (mod new-dial 100))
            (format t " steps: ~4d, times @0: ~2d, z: ~:[0~;1~] ~&dial: ~2d," steps times-through-100 (zerop new-dial) new-dial)
            
        :summing times-through-100 fixnum)) 

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-string infile-name))
         (data (parse-input input-lines)))
    (run parts data)))

(defun test (&rest parts)
  (run parts (parse-input *test-input*)))
