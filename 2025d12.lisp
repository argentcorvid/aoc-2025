;;;2025 day 12

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str))
  (add-package-local-nickname 'a 'alexandria-2))

(defparameter *day-number* 12)
(defparameter *input-name-template* "2025d~dinput.txt")

(defparameter *test-input*
  '())

(defparameter *verbose* nil)

(defun vformat (format-string &rest args)
  "print to standard output only if *verbose* is true"
  (when *verbose*
    (apply #'format t format-string args)))

(defun p1 (lines)
  (loop :for tree :in (remove-if-not (a:curry #'ppcre:scan "\\d+x\\d+") lines)
        :for (length width . requested) := (mapcar #'parse-integer (ppcre:split "x|(?::?\\s)" tree))
        :counting (>= (* length width) (reduce #'+ (mapcar (a:curry #'* 9) requested))))) 

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&No Part 2!")))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name)))
    (run parts input-lines)))

(defun test (&rest parts)
  (declare (ignore parts))
  (fresh-line)
  (princ "no test today, main is easier")
  0)
