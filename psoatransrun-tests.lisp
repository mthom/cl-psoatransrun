
(in-package #:psoatransrun-tests)

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

(defun process-answer-string (answer-string)
  (loop for item in (remove-duplicates
                     (sort (cons "no" (split-sequence #\Newline answer-string)) #'string<)
                     :test #'string=)
        for new-item = (string-trim '(#\Space #\Newline #\Backspace #\Tab
                                      #\Linefeed #\Page #\Return #\Rubout)
                                    item)
        when (string/= "" new-item)
          collect new-item))

(defun answers-match-p (expected-answers reported-answers)
  (let ((expected-answer-lines (process-answer-string expected-answers))
        (reported-answer-lines (process-answer-string reported-answers)))
    (equalp expected-answer-lines reported-answer-lines)))

(defun relative-filename (directory))

(defun run-test-suite ()
  (let ((subdirectories (list-subdirectories *test-suite-directory*)))
    (dolist (subdirectory subdirectories)
      (let* ((test-kb-filename (kb-pathname subdirectory)))
        (when (probe-file test-kb-filename)
          (format t "Running ~A test suite ...~%" (file-namestring test-kb-filename))

          (catch 'continue
            (let ((process (external-program:start *prolog-engine-path* nil
                                                   :input :stream
                                                   :output :stream)))
              (unwind-protect
                   (handler-case
                       (let ((psoa-kb-string (file-get-contents test-kb-filename)))
                         (multiple-value-bind (prolog-kb-string relationships
                                               is-relational-p prefix-ht)
                             (psoa-document->prolog psoa-kb-string)

                           (declare (ignore is-relational-p))
                           (init-prolog-process prolog-kb-string process)

                           (let ((engine-socket (connect-to-prolog-process process))
                                 (*all-solutions* t))
                             (loop for n upfrom 1
                                   for query-file-pathname = (query-pathname subdirectory n)
                                   for answer-file-pathname = (answer-pathname subdirectory n)
                                   if (probe-file query-file-pathname) do
                                     (let ((query-string (file-get-contents query-file-pathname))
                                           (answer-batch (file-get-contents answer-file-pathname)))
                                       (handler-case
                                           (unless
                                               (answers-match-p
                                                answer-batch
                                                (let ((*standard-output* (make-string-output-stream)))
                                                  (send-query-to-prolog-engine
                                                   (socket-stream engine-socket)
                                                   query-string
                                                   prefix-ht
                                                   relationships)
                                                  (get-output-stream-string *standard-output*)))
                                             (format t "Query ~D of ~A failed!~%"
                                                     n (file-namestring test-kb-filename)))
                                         (esrap:esrap-parse-error ()
                                           (format t "Parse error in query file ~A~%"
                                                   (file-namestring query-file-pathname)))))
                                   else do
                                     (format t "Finished testing ~A~%~%"
                                             (file-namestring test-kb-filename))
                                     (throw 'continue nil)))))

                     (esrap:esrap-parse-error ()
                       (format t "Parse error in KB file ~A~%~%"
                               (file-namestring test-kb-filename)))
                     (error (condition)
                       (format t "An error was received: ~A~%" condition)))
                (quit-prolog-engine process)))))))))
