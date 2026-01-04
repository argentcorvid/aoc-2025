;;;2025 day 11

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(:alexandria :str :serapeum))
  (add-package-local-nickname 'a 'alexandria-2)
  (add-package-local-nickname 's 'serapeum))

(defparameter *day-number* 11)
(defparameter *input-name-template* "2025d~dinput.txt")

(defparameter *test-input*
  (str:lines
   "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out"))

(defparameter *verbose* nil)

(defun vformat (format-string &rest args)
  "print to standard output only if *verbose* is true"
  (when *verbose*
    (apply #'format t format-string args)))

(defun parse-input (lines)
  (loop :with map = (s:dict)
        :for l :in lines
        :for sl = (ppcre:split ":? " l)
        :do (setf (gethash (first sl) map) (rest sl))
        :finally
           (format t "~&~a machines processed" (hash-table-count map))
           (return map)))


(defun dfs-all (start-state neighbor-func &key (end-state nil end-supplied-p))

  (prog ((queue (s:queue (list start-state)))
         (seen (s:dict))
         (path (list))
         (found-paths (list)))
   restart
     (when (s:queue-empty-p queue)
       (return-from dfs-all (nreverse found-paths)))
     (setf path (s:deq queue))
     (unless (and (equal (first path) end-state)
                  (gethash (first path) seen)) ;dont think this os right? - only finds one path
       (setf (gethash (first path) seen) t)
       (dolist (n (funcall neighbor-func (first path)))
         (s:undeq (append (list n) path) queue)))
     (when (equal (first path) end-state)
       (push (reverse path) found-paths))
     (go restart)))


(defun p1 (machines)
  (length (dfs-all "you" (lambda (k) (gethash k machines)) :end-state "out"))) 

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
