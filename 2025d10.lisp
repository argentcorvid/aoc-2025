;;;2025 day 10

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str :serapeum))
  (add-package-local-nickname 'a 'alexandria-2)
  (add-package-local-nickname 's 'serapeum))

(defparameter *day-number* 10)
(defparameter *input-name-template* "2025d~dinput.txt")

(defparameter *test-input*
  (str:lines "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"))

(defparameter *verbose* nil)

(defun vformat (format-string &rest args)
  "print to standard output only if *verbose* is true"
  (when *verbose*
    (apply #'format t format-string args)))

(defun parse-input (lines)
  (let ((scanner (ppcre:create-scanner "^\\[(.+)\\] (.+) \\{([\\d,]+)\\}$"))
        (out (list)))
    (dolist (line lines (nreverse out))
      (ppcre:register-groups-bind (lights button-actions joltage) (scanner line)
        (push (list (parse-lights lights)
                    (parse-actions button-actions (length lights))
                    (parse-joltage joltage))
              out)))))

(defun parse-lights (lights)
  (declare (string lights))
  (map '(vector bit) (lambda (c) (if (char= #\# c) 1 0)) lights))

(defun parse-actions (button-string width)
  (let ((out (list)))
    (dolist (button (ppcre:split "\\) ?" button-string) (nreverse out))
      (push (loop :for bit-pos :in (mapcar #'parse-integer (str:split #\, button :start 1))
                  :with button-bits = (make-array (list width) :element-type 'bit)
                  :do (setf (bit button-bits bit-pos) 1)
                  :finally (return button-bits) )
            out))))

(defun parse-joltage (joltage-string)
  (mapcar #'parse-integer (str:split #\, joltage-string)))

(declaim (inline press-button))

(defun press-button (light-state toggles)
  (bit-xor light-state toggles))

(defun bfs (start-state edges neighbor-func &key (end-state nil end-supplied-p))

  (let ((queue (s:queue (list start-state)))
        (seen (s:dict start-state t))
        (path (list)))
    (loop
      (when (s:queue-empty-p queue)
        (return-from bfs (nreverse path)))
      (setf path (s:deq queue))
      (when (and end-supplied-p (equal (first path) end-state))
        (return-from bfs (nreverse path)))
      (dolist (e edges)
        (let ((n (funcall neighbor-func (first path) e)))
          (unless (s:@ seen n)
            (setf (s:@ seen n) t)
            (s:enq (append (list n) path) queue)))))))

(defun p1 (machine-descriptions)
  (loop :for (required-state buttons) :in machine-descriptions
        :summing (1- (length (bfs (make-array (length required-state) :element-type 'bit)
                                  buttons
                                  #'press-button
                                  :end-state required-state))))) 

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
