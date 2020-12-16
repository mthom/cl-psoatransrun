
(in-package #:psoatransrun)

#|
#:psoatransrun defines two types of functions.

Functions of the first type communicate with the underlying logic
engine as a subprocess, a choice currently limited to either XSB
Prolog or Scryer Prolog. The implementation details of the subprocess
as a Lisp object are hidden behind the interface of the
#:prolog-engine-client package.

The engine client is used by #:psoatransrun as a bidirectional stream
to which query text is sent and from which answer bindings are received.

The operations defined in other packages of cl-psoatransrun are
applied sequentially in #:psoatransrun: PSOA RuleML KB and query
strings are read from an input stream, parsed, transformed, translated
to their Prolog counterparts, and sent to the engine; answer strings
are received in Prolog notation, and in turn translated to their PSOA
RuleML counterparts, which are reported back to the caller.

This infrastructure is modular enough that it is used by both the REPL
functions of #:psoatransrun and by the unit testing framework of
#:psoatransrun-tests.

The second type of functions are those composing the cl-psoatransrun
REPL, or read-eval-print-loop. The REPL functions are responsible for
loading the translated PSOA RuleML KB into the Prolog engine client,
and kickstarting the Prolog-side server, itself a Prolog program, that
evaluates PSOA RuleML queries it receives from cl-psoatransrun, and to
which it enumerates answer bindings.
|#


#|
Global variables for generalized boolean-valued options across
cl-psoatransrun:

*all-solutions* (defined below):

  If NIL, wait for a keypress before printing more solutions from the
logic engine backend.

*save-translated-kb* (defined below):

  Unless NIL, save the translated Prolog KB to the pathname stored in
this variable.

*static-objectification-only* (defined in psoa-transformers.lisp) :

  If t, use static undifferentiated objectification during the
objectify transformation.

*print-caret-before-expr* (defined in psoa-pprint.lisp) :

  If t, print exprs outside atoms with a preceding caret (^).
|#

(defparameter *all-solutions* nil
  "If NIL, wait for a keypress before printing more solutions from the
logic engine backend.")

(defparameter *save-translated-kb* nil
  "Unless NIL, save the translated Prolog KB to the pathname stored in
this variable.")


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
the KB is relational, and a hash table linking prefix namespaces to
URLs."
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
              (ruleml-document-prefix-ht document)))))

(defun psoa-query->prolog (query-string prefix-ht relationships)
  "Translate a ruleml-query value \"query\" to a Prolog query string,
similarly to the execution path of psoa-document->prolog."
  (-> (parse 'psoa-grammar::query (format nil "Query(~A)" query-string))
      (transform-query relationships prefix-ht)
      (translate-query prefix-ht)))

(defun trim-solution-string (solution-string)
  "Some logic engine backends format strings with enclosing
quotes. This function removes the quotes if found."
  (if (and (eql (char solution-string 0) #\")
           (eql (char solution-string (1- (length solution-string))) #\"))
      ;; The use of subseq is a kludge to remove quotation
      ;; marks printed by Scryer.
      (subseq solution-string 1 (1- (length solution-string)))
      solution-string))

(defun xsb-message-p (string)
  "A predicate function used to detect XSB warning messages in
\"string\". The XSB manual does not say whether it is possible to
prevent the printing of these warnings using, e.g., a command line
flag."
  (or (starts-with "++Warning" string)
      (starts-with "% " string)))

(defun read-and-collect-solutions (stream &optional (grammar 'prolog-grammar::goal-sequence))
  "Read lines of text representing answer bindings from \"stream\" and
collect them into the list \"solutions\" until the end of \"stream\"
is reached, a state signified by \"solutions\" being bound to NIL.

\"solutions\" may contain lists of ruleml-ast-node subtyped objects
produced by parsing the string against the esrap rule \"grammar\", an
optional argument whose default value is
'prolog-grammar::goal-sequence.

The default is appropriate for the cl-psoatransrun REPL, as answer
bindings are returned from the Prolog engine backend as strings of
comma-separated Prolog goals. The test suite, however, reads test case
answer bindings as strings of space-separated PSOA RuleML equations,
which demands a different grammar to be parsed."

  ;; Read characters into the string \"solution\" until a #\Newline is encountered.
  (loop for solution = (read-line stream nil nil)
        for trimmed-solution = (string-right-trim '(#\Return #\Newline) solution)
        if (null solution)
          collect "no" into solutions
          and do (return solutions)
        else if (equalp trimmed-solution "No")
          collect "no" into solutions
          and do (return solutions)
        else if (equalp trimmed-solution "Yes")
          collect "yes" into solutions
        else if (not (xsb-message-p trimmed-solution))
          collect (parse grammar (trim-solution-string trimmed-solution))
          into solutions))

(defun continue-reading-solutions-p (&optional (stream *standard-input*))
  "At the REPL, more answers are solicited by the hitting the #\; key;
if ENTER is hit instead, stop soliciting answers and return to the
prompt."
  (loop (match (read-char stream)
          (#\; (return t))
          ((or #\Return #\Newline)
           (terpri)
           (return nil))
          (_
           ;; remove trailing newline terminators from \"stream\"
           ;; following unrecognized characters.
           (read-line stream nil)))))

(defun print-solutions (solutions)
  "Print the solutions from the \"solutions\" list, formatted by the
type of each individual solution. Wait for a keypress before printing
the next solution when *all-solutions* is NIL; otherwise, print each
solution on a separate line."
  (loop for solution in solutions
        do (match solution
             ((type list)
              (format t "~{~A~^ ~}" solution))
             ("yes"
              (format t "yes~%"))
             ("no"
              (format t "no~%")
              (return)))
           (cond (*all-solutions* (terpri))
                 ((not (continue-reading-solutions-p))
                  (return)))))

(defun psoa-repl (engine-client prefix-ht
                  &optional (relationships (make-hash-table :test #'equalp)))
  "-psoa-repl contains the proper REPL, while this function catches
and prints diagnostic errors thrown during parsing. After parse errors
are caught and processed, the REPL resumes."
  (loop (handler-case (-psoa-repl engine-client prefix-ht relationships)
          (esrap:esrap-parse-error (condition)
            ;; If a PSOA RuleML query can't be parsed, print a brief
            ;; message giving the character offset where the parse
            ;; error occurred.
            (format t "Parse error at ~A at position ~D~%"
                    (esrap:esrap-error-text condition)
                    (esrap:esrap-error-position condition))))))

(defun print-prompt-and-read-line (stream)
  "Print the '>' prompt if the input stream \"stream\" does not offer
an immediately available character and read from \"stream\"."
  ;; If stream doesn't already have characters waiting to be read,
  ;; print the REPL prompt.
  (unless (listen stream)
    (write-string "> "))

  (read-line stream nil))

(defun -psoa-repl (engine-client prefix-ht relationships)
  "This function encapsulates a loop: read a query as a single line
from standard input, translate the query to a Prolog query string,
send it along to the engine, gather and print the solutions."
  (loop for line = (print-prompt-and-read-line *standard-input*)
        if line do (print-solutions (send-query-to-prolog-engine
                                     engine-client line
                                     prefix-ht relationships))))

(defun send-query-to-prolog-engine (engine-client query-string prefix-ht relationships)
  "Translate the PSOA RuleML query string \"query-string\" to its
Prolog equivalent, and send it out to the engine using its input
stream. Read and collect the answer bindings from the engine's output
stream."
  (multiple-value-bind (query-string toplevel-var-string)
      (psoa-query->prolog query-string prefix-ht relationships)
    (write-line query-string (prolog-engine-client-input-stream engine-client))
    (write-line toplevel-var-string (prolog-engine-client-input-stream engine-client))
    (force-output (prolog-engine-client-input-stream engine-client))

    (read-and-collect-solutions (prolog-engine-client-output-stream engine-client))))

(defun consult-local-file (filename stream)
  "Preface filename with the source directory (which means it is a
'local' file) and tell the Prolog engine to consult it."
  (write-string "consult('" stream)
  (write-string (namestring
                 (merge-pathnames (asdf:system-source-directory "psoatransrun")
                                  (pathname filename)))
                stream)
  (write-line   "')." stream)
  (finish-output stream))

(defun write-translated-kb-to-file (prolog-kb-string local-kb-pathname)
  "Write the translated Prolog KB \"prolog-kb-string\" to the file identified
by \"local-kb-pathname\"."
  (with-open-file (file-stream (namestring local-kb-pathname)
		   :direction :output :if-exists :supersede)
    (write-line prolog-kb-string file-stream)
    (finish-output file-stream)))

(defun init-prolog-process (engine-client prolog-kb-string process
                            &aux (system (prolog-engine-client-host engine-client)))
  "Load the translated Prolog KB into the Prolog engine backend by
writing \"prolog-kb-string\" to the process input stream. Next, load
the Prolog server corresponding to the Prolog system, whose execution
is begun by an :- initialization(...) directive."
  (let ((process-input-stream (uiop:process-info-input process))
        (system-servers '((:scryer . "scryer_server.pl")
                          (:xsb . "xsb_server.pl")))
        (local-kb-pathname (merge-pathnames (asdf:system-source-directory "psoatransrun")
                                            #p"local-KB.pl")))

    (write-translated-kb-to-file prolog-kb-string local-kb-pathname)

    ;; Compile the PSOA document in the engine.
    (consult-local-file "local-KB.pl" process-input-stream)

    ;; Load and start the server program, which is initialized
    ;; automatically within the server module via a ":-
    ;; initialization(...)."  directive.
    (consult-local-file (cdr (assoc system system-servers)) process-input-stream)

    ;; Set the process slot of the engine client.
    (set-prolog-engine-client-stream engine-client process)))

(defun psoa-load-and-repl (document &key (system :xsb))
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
start-prolog-process, initialize the KB using init-prolog-process, and
launch the REPL.

Upon abort or any other unresolvable error, execute quit-prolog-engine
to close the process."
  (multiple-value-bind (prolog-kb-string relationships
                        is-relational-p prefix-ht)
      (psoa-document->prolog document :system (prolog-engine-client-host engine-client))

    (loop
      (let ((*is-relational-p* is-relational-p)
            (process (start-prolog-process engine-client)))

        (format t "The translated KB:~%~%~A" prolog-kb-string)
	    (init-prolog-process engine-client prolog-kb-string process)

	    (when *save-translated-kb*
	      (write-translated-kb-to-file prolog-kb-string *save-translated-kb*))

	    (unwind-protect
	         ;; If a call beneath psoa-repl throws an error, the stack
	         ;; is unwound, resulting in this unwind-protect
	         ;; evaluating the (terminate-prolog-engine ...) form
	         ;; before passing control to the restart.
	         (psoa-repl engine-client prefix-ht relationships)
	      (terminate-prolog-engine engine-client))))))
