
;;;2025 day 6

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  (add-package-local-nickname 'a 'alexandria-2))

(defparameter *day-number* 6)
(defparameter *input-name-template* "2025d~dinput.txt")

(defparameter *test-input*
  (str:lines "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")
  "given answer is 4277556")

(defparameter *verbose* nil)

(defun vformat (format-string &rest args)
  "print to standard output only if *verbose* is true"
  (apply #'format t format-string args))

(defun parse-input (lines)
  (flet ((parse-int-but-op (line-in)
           (let ((split-line (str:split-omit-nulls #\space line-in)))
             (if  (find (elt split-line 0) '("+" "*") :test #'equal )
                  (mapcar #'a:ensure-symbol split-line)
                  (mapcar #'parse-integer split-line)))))
    (a:line-up-last
     lines
     (mapcar #'parse-int-but-op)
     (a:rotate)
     (apply #'mapcar #'list))))

(defun p1 (operations)
  (a:line-up-last
   operations
   (mapcar #'eval)
   (reduce #'+))) 

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
