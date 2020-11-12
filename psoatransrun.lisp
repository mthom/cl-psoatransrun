
(in-package #:psoatransrun)

#|
Global variables for command-line options.
|#

(defparameter *all-solutions* nil
  "If NIL, wait for a keypress before printing more solutions from the
logic engine backend.")


#|
Functions for translating PSOA RuleML documents and queries
to equivalent Prolog, and for sending those translations to
the Prolog engine and receiving back solutions.
|#

(defun psoa-document->prolog (document &key system)
  "Translate a ruleml-document value \"document\" to a Prolog KB
string, the first return value, first by applying transform-document
from #:psoa-transformers, and then by applying translate-document from
#:psoa-prolog-translator.

The remaining return values are the relationships hash table detailed
in the #:psoa-transformers documentation, a boolean indicating whether
the KB is relational, a hash table linking prefix namespaces to URLs,
and the original, untransformed ruleml-document AST."
  (check-type document string) ;; document is a string.
  (let* ((original-document (parse 'psoa-grammar::ruleml document))
         (document (transform-document original-document)))
    ;; document is now of type ruleml-document.
    (check-type document ruleml-document)
    (multiple-value-bind (prolog-kb-string relationships is-relational-p)
        (translate-document document :system system)
      (values prolog-kb-string
              relationships
              is-relational-p
              (ruleml-document-prefix-ht document)
              original-document))))

(defun psoa-query->prolog (query-string prefix-ht relationships)
  "Translate a ruleml-query value \"query\" to a Prolog query string,
similarly to the execution path of psoa-document->prolog."
  (flet ((recompile-for-relational-query (query-ast)
           ;; If the KB is relational but query is not, we should try
           ;; to recompile the KB non-relationally, but only if the
           ;; caller of psoa-query->prolog has set up an appropriate
           ;; context in which it can perform the re-compilation for
           ;; us.
           ;;
           ;; That is what the form (find-restart
           ;; 'recompile-non-relationally) detects. If no such context
           ;; exists, it evaluates to NIL and execution of
           ;; psoa-query->prolog continues as it normally
           ;; would. Otherwise, control is transferred to the nearest
           ;; restart named recompile-non-relationally, passing
           ;; query-string as its sole argument. Depending on the
           ;; establishment of the restart in the caller (via, i.e.,
           ;; restart-bind or restart-case), the call stack may be
           ;; unwound.
           (when (and *is-relational-p* (not (is-relational-query-p query-ast prefix-ht)))
             (let ((recompile-restart (find-restart 'recompile-non-relationally)))
               (when recompile-restart
                 (invoke-restart recompile-restart query-string))))
           query-ast))
    (-> (parse 'psoa-grammar::query (format nil "Query(~A)" query-string))
        recompile-for-relational-query
        (transform-query relationships prefix-ht)
        (translate-query prefix-ht))))

(defun trim-solution-string (solution-string)
  "Some logic engine backends format strings with enclosing
quotes. This function removes the quotes if found."
  (if (and (eql (char solution-string 0) #\")
           (eql (char solution-string (1- (length solution-string))) #\"))
      ;; The use of subseq is a kludge to remove quotation
      ;; marks printed by Scryer.
      (subseq solution-string 1 (1- (length solution-string)))
      solution-string))

(defun read-and-collect-solutions (stream &optional (grammar 'prolog-grammar::goal-sequence))
  "Read lines of text representing solution sets from \"stream\" and
collect them into the list \"solutions\" until the end of \"stream\"
is reached, a state signified by \"solutions\" being bound to NIL.

\"solutions\" may contain lists of ruleml-ast-node subtyped objects
produced by parsing the string against the esrap rule \"grammar\", an
optional argument whose the default value is
'prolog-grammar::goal-sequence.

The default is appropriate for the cl-psoatransrun REPL, as solution
sets are returned from the Prolog engine backend as strings of
comma-separated Prolog goals. The test suite, however, reads test case
solution sets as strings of space-separated PSOA RuleML equations,
which demands a different grammar to be parsed."
  (loop for solution = (read-line stream nil nil)
        if (null solution)
          collect "no" into solutions
          and do (return solutions)
        else if (equalp solution "No")
               collect "no" into solutions
               and do (return solutions)
        else if (equalp solution "Yes") do
          (read-line stream nil nil)
             and collect "yes" into solutions
             and do (return solutions)
        else collect
             (parse grammar (trim-solution-string solution))
          into solutions))

(defun print-solutions (solutions)
  "Print the solutions from the \"solutions\" list, formatted by the
type of each individual solution. Wait for a keypress before printing
the next solution when *all-solutions* is NIL; otherwise, print each
solution on a separate line."
  (loop for solution in solutions
        do (match solution
             ((type list)
              (format t "~{~A~^ ~}" solution))
             ((or "no" "yes")
              (format t "~A~%" solution)
              (return)))
           (if *all-solutions*
               (terpri)
               (read-char))))

(defun psoa-repl (engine-client prefix-ht
                  &optional (relationships (make-hash-table :test #'equalp)))
  "-psoa-repl contains the proper REPL, while this function catches
and prints diagnostic errors thrown during parsing. After parse errors
are caught and processed, the REPL resumes."
  (loop (handler-case (-psoa-repl engine-client prefix-ht relationships)
          (esrap:esrap-parse-error (condition)
            (format t "Parse error at ~A at position ~D~%"
                    (esrap:esrap-error-text condition)
                    (esrap:esrap-error-position condition))))))

(defun -psoa-repl (engine-client prefix-ht relationships)
  "This function encapsulates a loop: read a query as a single line
from standard input, translate the query to a Prolog query string,
send it along to the engine, gather and print the solutions."
  (loop for line = (let ((line (dequeue-query engine-client)))
                     (if line
                         line
                         (progn (write-string "> ")
                                (read-line *standard-input* nil))))
        if line do (print-solutions (send-query-to-prolog-engine
                                     engine-client line
                                     prefix-ht relationships))))

(defun send-query-to-prolog-engine (engine-client query-string prefix-ht relationships)
  "Translate the PSOA RuleML query string \"query-string\" to its
Prolog equivalent, and send it out to the engine using its output
socket. Read and collect the solution sets from the engine's output
socket."
  (let ((engine-socket (prolog-engine-client-socket engine-client)))
    (multiple-value-bind (query-string toplevel-var-string)
        (psoa-query->prolog query-string prefix-ht relationships)
      (write-line query-string (out-socket-stream engine-socket))
      (write-line toplevel-var-string (out-socket-stream engine-socket))
      (force-output (out-socket-stream engine-socket))
      (read-and-collect-solutions (in-socket-stream engine-socket)))))

(defun init-prolog-process (prolog-kb-string process &key system)
  "Load the translated Prolog KB into the Prolog engine backend by
writing \"prolog-kb-string\" to the process input stream. Next, load
the Prolog server corresponding to the Prolog system, whose execution
is begun by an :- initialization(...) directive."
  (let ((process-input-stream (sb-ext:process-input process))
        (system-servers '((:scryer . "scryer_server.pl")
                          (:xsb . "xsb_server.pl"))))
    ;; Compile the PSOA document in the engine.
    (write-line "[user]." process-input-stream)
    (finish-output process-input-stream)

    (write-line prolog-kb-string process-input-stream)
    (write-line "end_of_file." process-input-stream)
    (finish-output process-input-stream)

    ;; Loading the server engine, which is initialized automatically
    ;; within the module via a ":- initialization(...)." directive.
    (write-string "consult('" process-input-stream)
    (write-string "/home/mark/Projects/CL/PSOATransRun/" process-input-stream)
    (write-string (cdr (assoc system system-servers)) process-input-stream)
    (write-line   "')." process-input-stream)
    (finish-output process-input-stream)))


(defun quit-prolog-engine (process engine-client)
  "Terminate the Prolog engine, first by halting the server by writing
\"end_of_file.\" to its output socket, and then by writing \"halt.\"
to the process input stream. Wait for the process to close after closing
the process input and output streams."
  (write-line "end_of_file." (out-socket-stream (prolog-engine-client-socket engine-client)))
  (finish-output (out-socket-stream (prolog-engine-client-socket engine-client)))

  (close-socket (prolog-engine-client-socket engine-client))
  (reset-engine-socket engine-client)

  (write-line "halt." (sb-ext:process-input process))
  (finish-output (sb-ext:process-input process))

  (sb-ext:process-wait process)
  (sb-ext:process-close process)

  (close (sb-ext:process-input process) :abort t)
  (close (sb-ext:process-output process) :abort t))


(defun connect-to-prolog-process (engine-client process)
  "This function assumes that the Prolog engine has been initiated as
a subprocess by an invocation of init-prolog-process, and that the
server program is now running. This means it has printed the port number
the server is listening to, and is awaiting socket connections.

Therefore, connect-to-prolog-process tries to read and parse the port
integer from the subprocess' output stream, passing it along to
connect-to-prolog-process upon success. The error handling loop
surrounding the reading and parsing of the port number is escaped from
upon success."
  ;; It's possible for the runtime to print warning messages (ie.,
  ;; for singleton variables) in some cases. In those cases, ignore
  ;; the junk output and try to read the port again.
  (loop (handler-case
            (let* ((port (parse-integer (read-line (sb-ext:process-output process)))))
              (return-from connect-to-prolog-process
                (open-socket-to-prolog engine-client :port port)))
          (parse-error ()))))

(defun start-prolog-process (engine-client)
  "Launch the Prolog engine backend as a subprocess using the SBCL
run-program extension. Obviously this isn't portable to other Common
Lisp implementations."
  (sb-ext:run-program (prolog-engine-client-path engine-client)
                      '()
                      :input :stream
                      :output :stream
                      :wait nil))

(defun psoa-load-and-repl (document &key (system :scryer))
  "Try to find the executable of the Prolog backend in the filesystem,
with user assistance if necessary. Once done, enter -psoa-load-and-repl."
  (let ((engine-client (make-engine-client system)))
    (with-accessors ((path prolog-engine-client-path)) engine-client
      (if (and path (probe-file path))
          (-psoa-load-and-repl document engine-client)
          (progn (format t "Enter the path of ~A Prolog: " system)
                 (finish-output)
                 (setf path (probe-file (pathname (read-line))))
                 (psoa-load-and-repl document :system system))))))

(defun -psoa-load-and-repl (document engine-client)
  "Parse the PSOA RuleML \"document\", a string, into a PSOA RuleML document,
which is transformed and translated as described in the documentation
of psoa-document->prolog. Then launch the Prolog backend using
start-prolog-process, initialize the KB using init-prolog-process,
connect to the launched Prolog server using connect-to-prolog-process,
and launch the REPL.

Upon abort or any other unresolvable error, execute quit-prolog-engine
to close the process."
  (multiple-value-bind (prolog-kb-string relationships is-relational-p
                        prefix-ht document)
      (psoa-document->prolog document :system (prolog-engine-client-host engine-client))

    (loop
      (let ((*is-relational-p* is-relational-p))
        (restart-case
            (let* ((process (start-prolog-process engine-client)))

              (format t "The translated KB:~%~%~A" prolog-kb-string)

              (init-prolog-process prolog-kb-string process
                                   :system (prolog-engine-client-host engine-client))

              (connect-to-prolog-process engine-client process)

              (unwind-protect
                   ;; If a call beneath psoa-repl invokes the recompile-non-relationally
                   ;; restart, the stack is unwound, resulting in this unwind-protect
                   ;; evaluating the (quit-prolog-engine ...) form before passing
                   ;; control to the restart.
                   (psoa-repl engine-client prefix-ht relationships)
                (quit-prolog-engine process engine-client)))

          (recompile-non-relationally (instigating-query-string)
            ;; A non-relational query (here, instigating-query-string) can prompt
            ;; a non-relational re-compilation of the KB. That's precisely
            ;; what is done here, using the recompile-document-non-relationally
            ;; transform of #:psoa-transformers.
            (multiple-value-setq (prolog-kb-string relationships is-relational-p)
              (translate-document (recompile-document-non-relationally document)
                                  :system (prolog-engine-client-host engine-client)))

            ;; *is-relational-p* must be bound to NIL when we are done here.
            ;; Raise an error if this is not so.
            (assert (not is-relational-p))

            ;; enqueue the instigating query for execution as soon as the Prolog
            ;; server is restarted with the re-compiled KB.
            (enqueue-query engine-client instigating-query-string)))))))
