
(in-package #:psoatransrun-tests)

(named-readtables:in-readtable rutils-readtable)

(defparameter *test-suite-directory*
  "/home/mark/git/PSOATransRunComponents/PSOATransRun/test/")

(defun file-pathname (containing-dir-pathname filename)
  (merge-pathnames containing-dir-pathname (pathname filename)))

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun kb-pathname (subdirectory)
  (file-pathname subdirectory
                 (format nil "~A-KB.psoa" (directory-name subdirectory))))

(defun query-pathname (subdirectory n)
  (file-pathname subdirectory
                 (format nil "~A-query~D.psoa"
                         (directory-name subdirectory) n)))

(defun answer-pathname (subdirectory n)
  (file-pathname subdirectory
                 (format nil "~A-answer~D.psoa"
                         (directory-name subdirectory) n)))

(defun list-subdirectories (pathname)
  (remove-if-not #'directory-p
                 (directory (make-pathname :name :wild :type :wild
                                           :defaults pathname))))

(defun nullify-node-positions (answer-list)
  (mapcar #`(typecase %
              (list (mapcar #`(transform-ast
                               %
                               (lambda (node &key &allow-other-keys)
                                 (setf (ruleml-ast-node-position node) 0)
                                 node))
                            %))
              (t %))
          answer-list))

(defun equal-answer-set-p (answer-set-1 answer-set-2)
  (cond
    ((and (typep answer-set-1 'string) (typep answer-set-2 'string))
     (string= answer-set-1 answer-set-2))
    ((and (typep answer-set-1 'list) (typep answer-set-2 'list))
     (not (set-exclusive-or answer-set-1 answer-set-2
                            :test #'equalp)))
    (t
     nil)))

(defun answers-match-p (expected-answers reported-answers)
  (let ((expected-answers (nullify-node-positions expected-answers))
        (reported-answers (nullify-node-positions reported-answers)))
    (every #'equal-answer-set-p expected-answers reported-answers)))

(defun run-test-case (test-kb-filename subdirectory engine-client
                      process prefix-ht relationships)
  (let ((engine-socket (connect-to-prolog-process engine-client process))
        (*all-solutions* t))
    (unwind-protect
         (loop for n upfrom 1
               for query-file-pathname = (query-pathname subdirectory n)
               for answer-file-pathname = (answer-pathname subdirectory n)
               if (probe-file query-file-pathname) do
                 (let ((query-string (file-get-contents query-file-pathname))
                       (answer-batch (file-get-contents answer-file-pathname)))
                   (handler-case
                       (unless
                           (answers-match-p
                            (read-and-collect-solutions
                             (make-string-input-stream answer-batch)
                             'psoa-grammar::formula-list)
                            (send-query-to-prolog-engine
                             engine-socket
                             query-string
                             prefix-ht
                             relationships))
                         (format t "Query ~D of ~A failed!~%"
                                 n (file-namestring test-kb-filename)))
                     (esrap:esrap-parse-error ()
                       (format t "Parse error in query file ~A~%"
                               (file-namestring query-file-pathname)))))
               else do
                 (format t "Finished testing ~A~%~%"
                         (file-namestring test-kb-filename))
                 (throw 'continue nil))
      (close-socket engine-socket))))

(defun run-test-suite (&key (system :scryer))
  (let ((subdirectories (list-subdirectories *test-suite-directory*))
        (engine-client (make-engine-client system)))
    (dolist (subdirectory subdirectories)
      (let* ((test-kb-filename (kb-pathname subdirectory)))
        (when (probe-file test-kb-filename)
          (format t "Running ~A test suite ...~%" (file-namestring test-kb-filename))
          (catch 'continue
            (handler-case
                (let ((psoa-kb-string (file-get-contents test-kb-filename)))
                  (multiple-value-bind (prolog-kb-string relationships
                                        is-relational-p prefix-ht)
                      (psoa-document->prolog psoa-kb-string :system system)
                    (declare (ignore is-relational-p))
                    (let ((process (start-prolog-process engine-client)))
                      (unwind-protect
                           (progn (init-prolog-process prolog-kb-string process :system system)
                                  (run-test-case test-kb-filename subdirectory engine-client
                                                 process prefix-ht relationships))
                        (quit-prolog-engine process)))))
              (esrap:esrap-parse-error ()
                (format t "Parse error in KB file ~A~%~%"
                        (file-namestring test-kb-filename)))
              (error (condition)
                (format t "An error was received: ~A~%~%" condition)))))))))
