
(in-package #:psoatransrun-tests)

(named-readtables:in-readtable rutils-readtable)

(defparameter *test-suite-directory*
  #p"/home/mark/git/PSOATransRunComponents/PSOATransRun/test/"
  "The path to PSOATransRunComponents test suite. Change this to point
  to your local copy!")

(defun file-pathname (containing-dir-pathname filename)
  "Conjoins \"filename\" to the directory \"pathname\" and returns the
resulting pathname."
  (merge-pathnames containing-dir-pathname (pathname filename)))

(defun file-get-contents (filename)
  "Slurps the contents of a file to a string, which is returned."
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun kb-pathname (subdirectory)
  "The test suite KB name is always derived from the name of its containing subdirectory,
appending \"-KB.psoa\" to produce a filename."
  (file-pathname subdirectory
                 (format nil "~A-KB.psoa" (directory-name subdirectory))))

(defun query-pathname (subdirectory n)
  "Query files are labeled with an integer \"n\" and count upwards."
  (file-pathname subdirectory
                 (format nil "~A-query~D.psoa"
                         (directory-name subdirectory) n)))

(defun answer-pathname (subdirectory n)
  "Answer files are labeled with an integer \"n\" and count upwards."
  (file-pathname subdirectory
                 (format nil "~A-answer~D.psoa"
                         (directory-name subdirectory) n)))

(defun list-subdirectories (pathname)
  "List the subdirectories of the directories at \"pathname\"."
  (remove-if-not #'directory-p
                 (directory (make-pathname :name :wild :type :wild
                                           :defaults pathname))))

(defun equal-answer-set-p (answer-set-1 answer-set-2)
  "Answer sets are are either strings or lists of PSOA RuleML AST nodes.
To be equal, \"answer-set-1\" and \"answer-set-2\" must share one of
these types. If they are both strings, they are checked for equality.
If they are both lists of AST nodes, they are pretty printed to
strings and checked for equality modulo list ordering via the function
set-exclusive-or."
  (cond
    ((and (typep answer-set-1 'string) (typep answer-set-2 'string))
     (string= answer-set-1 answer-set-2))
    ((and (typep answer-set-1 'list) (typep answer-set-2 'list))
     (not (set-exclusive-or (mapcar #`(format nil "~A" %) answer-set-1)
                            (mapcar #`(format nil "~A" %) answer-set-2)
                            :test #'string=)))
     (t
      nil)))

(defun answers-match-p (expected-answers reported-answers)
  "Two answers set match iff all their corresponding answer sets match."
  (not (set-exclusive-or expected-answers reported-answers
                         :test #'equal-answer-set-p)))

(defun run-test-case (test-kb-filename subdirectory engine-client
                      prefix-ht relationships)
  "Iterate through query and answer files, send them to the Prolog
engine client, get back the answers, and compare them with the answers
expected by the test case in the corresponding answer file. Print a
message when a query fails."
  (let ((*all-solutions* t))
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
                        engine-client
                        query-string
                        prefix-ht
                        relationships))
                    (format t "Query ~D of ~A failed!~%"
                            n (file-namestring test-kb-filename)))
                (esrap:esrap-parse-error (condition)
                  (format t "Parse error at \"~A\" at position ~D~%"
                          (esrap:esrap-error-text condition)
                          (esrap:esrap-error-position condition)))))
          else do
            (format t "Finished testing ~A~%~%"
                    (file-namestring test-kb-filename))
            (throw 'continue nil))))

(defun run-test-suite (&key (system :xsb))
  "Create and destroy a fresh instance of the Prolog engine derived
from the \"system\" keyword for each test case in
*test-suite-directory*. For now, print parse errors and other error
types as notifications to the screen, but don't allow them to prevent
further tests."
  (let ((subdirectories (list-subdirectories *test-suite-directory*))
        (engine-client (make-engine-client system))
        (psoa-pprint::*print-caret-before-expr* nil))
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
                           (progn (init-prolog-process engine-client prolog-kb-string process)
                                  (run-test-case test-kb-filename subdirectory engine-client
                                                 prefix-ht relationships))
                        (quit-prolog-engine engine-client)))))
              (esrap:esrap-parse-error ()
                (format t "Parse error in KB file ~A~%~%"
                        (file-namestring test-kb-filename)))
              (error (condition)
                (format t "An error was received: ~A~%~%" condition)))))))))
