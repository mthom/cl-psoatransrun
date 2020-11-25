
(in-package #:psoa-prolog-translator)

#|
#:psoa-prolog-translator converts ruleml-document and ruleml-query
ASTs to strings representing their Prolog KB and query
counterparts. It has the additional responsibility of tailoring those
KBs and queries to meet the specific requirements of the targeted
Prolog engine.
|#

(named-readtables:in-readtable rutils-readtable)

(defun translate-query (query prefix-ht)
  "Translate a ruleml-query AST to a string representation of
its counterpart Prolog query, the first return value of translate-query.

The second return value of translate-query is the list of generated
variables contained in the ruleml-query. These must be sent to the
server running atop the Prolog engine that will evaluate the query and
send back the variable equations defining its solutions. The server
must be told which variables are generated so as to exclude them from
its reported solutions."
  (let* ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
         (query-vars)
         (output-stream (make-string-output-stream)))
    (transform-ast (ruleml-query-term query)
                   (lambda (term &key &allow-other-keys)
                     (when (ruleml-genvar-p term)
                       (pushnew (ruleml-var-name term) query-vars))
                     term))
    (translate-item (ruleml-query-term query)
                    prefix-ht
                    output-stream
                    nil)
    (values (format nil "~A." (get-output-stream-string output-stream))
            (format nil "[~{Q~A~^, ~}]." query-vars))))

(defun sort-kb (prolog-kb-string predicate-indicators)
  "Split the prolog-kb-string into its constituent lines and sort them
according to the lexicographical order of their predicate indicators,
which are dotted pairs of the form

(predicate-name . predicate-arity)

Some Prolog backends like Scryer lack support for discontiguous
predicates, and so any Prolog KBs given to them can only contain only
predicate declarations whose clauses are listed contiguously."
  (let* ((stream (make-string-input-stream prolog-kb-string))
         ;; lines is a pairing of each line with its predicate
         ;; indicator, since the list predicate-indicators is already
         ;; sorted in order of occurrence, i.e., the ith predicate
         ;; indicator corresponds to the ith line of the KB, if no
         ;; empty lines or NIL predicate indicators appeared prior.
         (lines (loop for line = (read-line stream nil)
                      for pred-ind in predicate-indicators
                      if (and line pred-ind)
                        collect (cons line pred-ind))))
    (nreverse
     (remove-duplicates
      (mapcar #'car
              (sort lines (lambda (p1 p2)
                            ;; Sort lexicographically by predicate indicator.
                            (or (string< (car p1) (car p2))
                                (and (<= (cdr p1) (cdr p2))
                                     (string= (car p1) (car p2)))))
                    :key #'cdr))
      :test #'equal))))


(defun translate-document (document &key system)
  "Converts a document (a RuleML AST of type ruleml-document) to a
string representing the equivalent Prolog KB. The 'system' argument is
used to accommodate different declarative requirements across Prolog
systems."
  (multiple-value-bind (prolog-kb-string relationships
                        is-relational-p predicate-indicators)
      (-translate-document document)
    (ecase system
      (:scryer (let ((collated-stream (make-string-output-stream))
                     ;; Scryer doesn't yet support discontiguous predicates,
                     ;; so sort the predicate lines by their predicate indicators,
                     ;; so that all predicate clauses are printed contiguously.
                     (lines (sort-kb prolog-kb-string predicate-indicators))
                     (predicate-indicators (alist->ht (mapcar #`(cons % t) predicate-indicators)
                                                      :test #'equalp)))
                 ;; For Scryer, include the tabling library ...
                 (format collated-stream ":- use_module(library(tabling)).~%~%")
                 ;; ... as well as tabling declarations for each unique
                 ;; predicate indicator.
                 (loop for key being each hash-key of predicate-indicators
                       if (and (car key) (cdr key))
                         do (format collated-stream ":- table ~A/~A.~%"
                                    (car key) (cdr key)))
                 (format collated-stream "~%")
                 (dolist (line lines)
                   (format collated-stream "~A~%" line))
                 ;; Return the KB string, the relationships hash table, and a flag
                 ;; indicating whether the KB is wholly relational.
                 (values (get-output-stream-string collated-stream)
                         relationships
                         is-relational-p)))
      (:xsb (let ((collated-stream (make-string-output-stream))
                  (lines (sort-kb prolog-kb-string predicate-indicators)))
              ;; For XSB, the only advance declaration needed is
              ;; :- auto_table and datime for the current date and time.
              (format collated-stream ":- use_module(standard, [local_datime/1, datime/1]).~%~%")
              (format collated-stream ":- auto_table.~%~%")
              (dolist (line lines)
                (format collated-stream "~A~%" line))
              (values (get-output-stream-string collated-stream)
                      relationships
                      is-relational-p))))))

(defun -translate-document (document)
  "Iterates through the performatives of a ruleml-document object
and calls the line translator translate-item on each item of a Query
or Assert. In the process, it gathers predicate indicator dotted pairs
for later sorting purposes."
  (let* (;; Since Prolog is the translated target, disable the action
         ;; of the pretty printer on the AST nodes.
         (*print-pprint-dispatch* (copy-pprint-dispatch nil))
         (prolog-kb-stream (make-string-output-stream))
         (performatives (ruleml-document-performatives document))
         (predicate-indicators)
         (prefix-ht (ruleml-document-prefix-ht document))
         (relationships)
         (is-relational-p))
    (dolist (performative performatives)
      (match performative
        ((ruleml-assert :items items :relationships assert-relationships
                        :is-relational-p assert-is-relational-p)
         ;; if relationships is NIL, this is the first Assert we've encountered
         ;; in performatives.
         (unless (null relationships)
           (error "multiple Assert's in a single PSOA KB isn't yet supported"))
         (setf relationships assert-relationships
               is-relational-p assert-is-relational-p)
         (dolist (item items)
           ;; translate-item returns a single predicate indicator describing the top-level
           ;; predicate it translated and wrote to prolog-kb-stream.
           (let ((item-predicate-indicator (translate-item item prefix-ht prolog-kb-stream)))
             (push item-predicate-indicator predicate-indicators)
             (format prolog-kb-stream ".~%"))))
        ((ruleml-query :term query-term)
         (format prolog-kb-stream "?- ~A."
                 (translate-item query-term prefix-ht prolog-kb-stream)))))
    (values (get-output-stream-string prolog-kb-stream)
            relationships
            is-relational-p
            (nreverse predicate-indicators))))

(defun translate-item (item prefix-ht stream &optional (assert-item-p t))
  "Translate a single KB item (either an Assert item or Query formula)
to its Prolog transformation and write the result to the output stream
\"stream\". If assert-item-p is true, predicate-indicator information
should be recorded to the local variable of that name.

The translations of the atom templates matched in the translate local
function and their target templates in Prolog are specified in table
8.2 of Gen Zou's PhD thesis."
  (let (predicate-indicators)
    (macrolet ((record-predicate-indicator (name arity recordp)
                 ;; A local macro, setting the write-once local
                 ;; variable predicate-indicators to the cons
                 ;; cell/dotted pair of the form (name . arity).
                 `(when ,recordp
                    (setf predicate-indicators (cons ,name ,arity)))))
      (labels ((translate (item &optional stream recordp)
                 "This local function generates individual Prolog
atoms for each PSOA RuleML atom/AST node/\"item\" it
processes. \"stream\" is allowed to assume the NIL value when
translate is called by itself.  A NIL stream value passed as format's
first argument instructs it to print to a string instead of a proper
stream, which it returns. These strings are then inserted into the
corresponding arguments of the format strings of the root translate
call, where stream is bound to a proper stream object.

\"recordp\" is similarly NIL unless its invocation of translate was
made from outside of translate. It being set to t causes the
predicate-indicator cell to be written."
                 (match item
                   ((ruleml-oidful-atom
                     :oid oid
                     :predicate (ruleml-atom :root (ruleml-const :contents "Top")
                                             :descriptors (list (ruleml-tuple :terms terms))))
                    ;; This is an oidful atom with a single (either
                    ;; dependent or independent!) descriptor, hence
                    ;; we use "tupterm" containing just the OID and
                    ;; however many arguments are in the tuple. We
                    ;; should record the predicate indicator
                    ;; "tupterm"/(1+ (length terms)) to the
                    ;; predicate-indicators cell if this atom exists
                    ;; at the top-level of a KB.

                    ;; Because record-predicate-indicator is a macro
                    ;; and not a function, the form (1+ (length terms))
                    ;; isn't evaluated unless recordp is t.
                    (record-predicate-indicator "tupterm" (1+ (length terms)) recordp)
                    (if terms
                        (format stream "tupterm(~A, ~{~A~^, ~})"
                                (translate oid)
                                (mapcar #'translate terms))
                        (format stream "tupterm(~A)"
                                (translate oid))))
                   ((ruleml-oidful-atom
                     :oid oid
                     :predicate (ruleml-atom :root root
                                             :descriptors (list (ruleml-tuple :dep t
                                                                              :terms terms))))
                    ;; Similarly for prdtupterm, and subsequent patterns.
                    (record-predicate-indicator "prdtupterm" (+ 2 (length terms)) recordp)
                    (if terms
                        (format stream "prdtupterm(~A, ~A, ~{~A~^, ~})"
                                (translate oid)
                                (translate root)
                                (mapcar #'translate terms))
                        (format stream "prdtupterm(~A, ~A)"
                                (translate oid)
                                (translate root))))
                   ((ruleml-oidful-atom
                     :oid oid
                     :predicate (ruleml-atom :root (ruleml-const :contents "Top")
                                             :descriptors (list (ruleml-slot :name name
                                                                             :filler filler))))
                    (record-predicate-indicator "sloterm" 3 recordp)
                    (format stream "sloterm(~A, ~A, ~A)"
                            (translate oid)
                            (translate name)
                            (translate filler)))
                   ((ruleml-oidful-atom
                     :oid oid
                     :predicate (ruleml-atom :root root
                                             :descriptors (list (ruleml-slot :dep t
                                                                             :name name
                                                                             :filler filler))))
                    (record-predicate-indicator "prdsloterm" 4 recordp)
                    (format stream "prdsloterm(~A, ~A, ~A, ~A)"
                            (translate oid)
                            (translate root)
                            (translate name)
                            (translate filler)))
                   ((ruleml-oidful-atom
                     :oid oid
                     :predicate (ruleml-atom :root root :descriptors descriptors))
                    (record-predicate-indicator "prdtupterm" (+ 2 (length descriptors)) recordp)
                    (if descriptors
                        (format stream "prdtupterm(~A, ~A, ~{~A~^, ~})"
                                (translate oid)
                                (translate root)
                                (mapcar #'translate descriptors))
                        (format stream "prdtupterm(~A, ~A)"
                                (translate oid)
                                (translate root))))
                   ((ruleml-membership :oid oid :predicate predicate)
		            (match predicate
		              ((ruleml-const :contents "Top")
		               (format stream "true"))
		              (_
		               (record-predicate-indicator "memterm" 2 recordp)
		               (format stream "memterm(~A, ~A)"
			                   (translate oid)
			                   (translate predicate)))))
                   ((or (ruleml-atom :root root :descriptors (list (ruleml-tuple :dep t :terms terms)))
                        (ruleml-expr :root root :terms (list (ruleml-tuple :dep t :terms terms))))
                    (let ((root-string (translate root)))
                      (record-predicate-indicator root-string (length terms) recordp)
                      (if terms
                          (format stream "~A(~{~A~^, ~})"
                                  root-string
                                  (mapcar #'translate terms))
                          (format stream "~A"
                                  root-string))))
                   ((ruleml-expr :root root :terms terms)
                    (let ((root-string (translate root)))
                      (record-predicate-indicator root-string (length terms) recordp)
                      (if terms
                          (format stream "~A(~{~A~^, ~})"
                                  root-string
                                  (mapcar #'translate terms))
                          (format stream "~A"
                                  root-string))))
                   ((ruleml-equal :left left :right (ruleml-external :atom atom))
                    (format stream "is(~A, ~A)"
                            (translate left)
                            (translate atom)))
                   ((ruleml-equal :left left :right right)
                    (format stream "'='(~A, ~A)"
                            (translate left)
                            (translate right)))
                   ((ruleml-and :terms terms)
                    (format stream "(~{~A~^, ~})"
                            (remove nil (mapcar #'translate terms))))
                   ((ruleml-or :terms terms)
                    (if terms
                        (format stream "(~{~A~^; ~})"
                                (mapcar #'translate terms))
                        (format stream "false")))
                   ((or (ruleml-exists :formula formula)
                        (ruleml-forall :clause formula))
                    (translate formula stream assert-item-p))
                   ((ruleml-external :atom atom)
                    (translate atom stream))
                   ((ruleml-implies :conclusion conclusion :condition condition)
                    (format stream "~A :- ~A"
                            (translate conclusion nil t) ;; Conclusions formulas are at top-level..
                            (translate condition))) ;; .. and condition formulas are not.
                   ((ruleml-var :name name)
                    (format stream "Q~A" name))
                   ((ruleml-naf :formula formula)
                    (format stream "\\+ (~A)"
                            (translate formula)))
                   ((ruleml-const :contents const)
                    (match const
                      ((ruleml-pname-ln :name ns :url local)
                       ;; Translate prefixed predicates
                       ;; using the helper functions defined in
                       ;; psoa-ast.lisp.
                       (record-predicate-indicator
                        (write-url-const ns local prefix-ht)
                        0
                        recordp)
                       (write-url-const ns local prefix-ht stream))
                      ((type string)
                       (record-predicate-indicator const 0 recordp)
                       (if (eql (char const 0) #\_)
                           (format stream "'~A'" const)
                           (format stream "'_~A'" const)))
                      ((type number)
                       (format stream "~A" const))))
                   ((ruleml-number :value number)
                    (format stream "~A" number))
                   ((ruleml-iri :contents iri)
                    (format stream "'<~A>'" iri))
                   ((ruleml-string :contents const)
                    ;; Enclose strings in double quotes.
                    (format stream "\"~A\"" const)))))
        (translate item stream assert-item-p)
        predicate-indicators))))
