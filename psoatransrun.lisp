
(in-package #:psoatransrun)

(defparameter *prolog-engine-path* #p"/home/mark/Projects/Rust/scryer-prolog/target/release/scryer-prolog")

(defun psoa-document->prolog (document)
  (-> (parse 'psoa-grammar::ruleml document)
      transform-document
      translate-document))

(defun psoa-query->prolog (query &optional (relationships (make-hash-table :test #'equalp)))
  (-> (parse 'psoa-grammar::query (format nil "Query(~A)" query))
      (transform-query relationships)
      translate-query))

(defun read-and-print-solutions (engine-socket)
  (loop for solution = (read-line (socket-stream engine-socket) nil nil)
        do (cond ((null solution)
                  (return-from read-and-print-solutions))
                 ((equalp solution "no")
                  (format t "~%no~%")
                  (return-from read-and-print-solutions))
                 ((equalp solution "")
                  (format t "~%yes~%"))
                 (t
                  (write-string (subseq solution 1 (1- (length solution)))) ;; remove quotes inserted by Scryer
                  ))
           (read-char)))

(defun psoa-repl (engine-socket &optional (relationships (make-hash-table :test #'equalp)))
  (loop for line = (progn (write-string "> ")
                          (read-line *standard-input* nil))
        if line do
          (format t "~A~%" (psoa-query->prolog line relationships))
          (write-line (psoa-query->prolog line relationships) (socket-stream engine-socket))
          (force-output (socket-stream engine-socket))
          (read-and-print-solutions engine-socket)))

(defun psoa-load-and-repl (document)
  (with ((prolog-kb-string relationships (psoa-document->prolog document))
         (process (start *prolog-engine-path* nil
                         :input :stream
                         :output :stream)))
    (format t "The translated KB:~%~%~A" prolog-kb-string)

    ;; Compile the PSOA document in the engine.
    (write-line "[user]." (process-input-stream process))
    (write-line prolog-kb-string (process-input-stream process))
    (write-line "end_of_file." (process-input-stream process))
    (finish-output (process-input-stream process))

    ;; Opening the server engine, initialized automatically within the module using
    ;; an :- initialization(...) directive.
    (write-line "use_module('scryer_server.pl')." (process-input-stream process))
    (finish-output (process-input-stream process))

    ;; Dispense with "?-" junk.
    (read-line (process-output-stream process))

    (with ((port (parse-integer (read-line (process-output-stream process))))
           (engine-socket (socket-connect "127.0.0.1" port)))
      (psoa-repl engine-socket relationships))))
