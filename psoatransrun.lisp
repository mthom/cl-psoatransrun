
(in-package #:psoatransrun)

(defparameter *prolog-engine-path* #p"/home/mark/Projects/Rust/scryer-prolog/target/release/scryer-prolog")

(defun psoa-document->prolog (document)
  (-> (parse 'psoa-grammar::ruleml document)
      transform-document
      translate-document))

(defun psoa-query->prolog (query prefix-ht &optional (relationships (make-hash-table :test #'equalp)))
  (-> (parse 'psoa-grammar::query (format nil "Query(~A)" query))
      (transform-query relationships)
      (translate-query prefix-ht)))

(defun read-and-print-solutions (engine-socket)
  (loop for solution = (read-line (socket-stream engine-socket) nil nil)
        do (cond ((null solution)
                  (return-from read-and-print-solutions))
                 ((equalp solution "no")
                  (format t "~%No~%")
                  (return-from read-and-print-solutions))
                 ((equalp solution "[]")
                  (format t "~%Yes "))
                 (t
                  ;; the use of subseq is a kludge to remove quotation marks printed by Scryer
                  (write-string (subseq solution 1 (1- (length solution))))))
           (read-char)))

(defun psoa-repl (engine-socket prefix-ht &optional (relationships (make-hash-table :test #'equalp)))
  (loop (handler-case (-psoa-repl engine-socket prefix-ht relationships)
          (esrap:esrap-parse-error (condition)
            (format t "Parse error at ~A at position ~D~%"
                    (esrap:esrap-error-text condition)
                    (esrap:esrap-error-position condition))))))

(defun -psoa-repl (engine-socket prefix-ht &optional (relationships (make-hash-table :test #'equalp)))
  (loop for line = (progn (write-string "> ")
                          (read-line *standard-input* nil))
        if line do
          (format t "~A~%" (psoa-query->prolog line prefix-ht relationships))
          (write-line (psoa-query->prolog line prefix-ht relationships) (socket-stream engine-socket))
          (force-output (socket-stream engine-socket))
          (read-and-print-solutions engine-socket)))

(defun psoa-load-and-repl (document)
  (with ((prolog-kb-string relationships prefix-ht (psoa-document->prolog document))
         (process (start *prolog-engine-path* nil
                         :input :stream
                         :output :stream)))
    (format t "The translated KB:~%~%~A" prolog-kb-string)

    ;; Compile the PSOA document in the engine.
    (write-line "[user]." (process-input-stream process))
    (write-line prolog-kb-string (process-input-stream process))
    (write-line "end_of_file." (process-input-stream process))
    (finish-output (process-input-stream process))

    ;; Loading the server engine, which is initialized automatically
    ;; within the module via a ":- initialization(...)." directive.
    (write-line "use_module('scryer_server.pl')." (process-input-stream process))
    (finish-output (process-input-stream process))

    ;; It's possible for the runtime to print warning messages (ie.,
    ;; for singleton variables) in some cases. In those cases, ignore
    ;; the junk output and try to read the port again.
    (loop (handler-case
              (with ((port (parse-integer (read-line (process-output-stream process))))
                     (engine-socket (socket-connect "127.0.0.1" port)))
                (psoa-repl engine-socket prefix-ht relationships))
            (parse-error ())))))
