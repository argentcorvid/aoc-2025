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

(defparameter *test-input-2*
  (str:lines
   "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out"))

(defparameter *verbose* nil)

(defun vformat (format-string &rest args)
  "print to standard output only if *verbose* is true"
  (when *verbose*
    (apply #'format t format-string args)))

(defun parse-input (lines)
  (loop :with map = (s:dict)
        :for l :in lines
        :for (k . v) = (ppcre:split ":? " l)
        :do (setf (gethash k map) v)
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

(defvar *cache* (make-hash-table :test 'equal))

(defun count-paths (current dest)
  (declare (special machines))
  (let ((cache-key (list current dest)))
    (cond ((gethash cache-key *cache*))
          ((string= current dest)
           (setf (gethash cache-key *cache*) 1))
          ((setf (gethash cache-key *cache*)
                 (loop :for next :in (gethash current machines)
                       :summing (count-paths next dest)))))))

(defun p1 (machines)
  (declare (special machines))
  (count-paths "you" "out")) 

(defun p2 (machines)
  (declare (special machines))
  (labels ((combine-paths (starts ends)
             (reduce #'* (mapcar #'count-paths starts ends))))
    (let ((start-groups '(("svr" "fft" "dac")
                          ("svr" "dac" "fft")))
          (end-groups   '(("fft" "dac" "out")
                          ("dac" "fft" "out"))))
      (reduce #'+ (mapcar #'combine-paths
                          start-groups end-groups)))))

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
    (dolist (part (a:ensure-list parts))
      (ccase part
        (1 (run 1 (parse-input *test-input*)))
        (2 (run 2 (parse-input *test-input-2*)))))))
