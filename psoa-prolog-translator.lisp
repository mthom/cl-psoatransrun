
(in-package #:psoa-prolog-translator)

(named-readtables:in-readtable rutils-readtable)

(defun translate-query (query)
  (let* ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
         (output-stream (make-string-output-stream)))
    (translate- (ruleml-query-term query)
                (make-hash-table :test #'equalp)
                output-stream
                nil)
    (format nil "?- ~A."
            (get-output-stream-string output-stream))))

(defun translate-document (document &key (system :scryer))
  (multiple-value-bind (prolog-kb-string relationships predicate-indicators)
      (translate-document- document)
    (ecase system
      (:scryer (let* ((stream (make-string-input-stream prolog-kb-string))
                      (collated-stream (make-string-output-stream))
                      (lines (loop for line = (read-line stream nil)
                                   while line collect line))
                      (lines (sort lines #'string<=)))
                 (format collated-stream ":- use_module(library(tabling)).~%~%")
                 (loop for key being each hash-key of predicate-indicators
                       do (format collated-stream ":- table ~A/~A.~%"
                                  (lt key) (rt key)))
                 (format collated-stream "~%")
                 (dolist (line lines)
                   (format collated-stream "~A~%" line))
                 (values (get-output-stream-string collated-stream)
                         relationships))))))

(defun translate-document- (document)
  (let* ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
         (prolog-kb-stream (make-string-output-stream))
         (performatives (ruleml-document-performatives document))
         (prefixes (ruleml-document-prefixes document))
         (prefix-ht (alist->ht (loop for prefix in prefixes
                                     collect (cons (ruleml-prefix-name prefix)
                                                   (ruleml-prefix-iri-ref prefix)))
                               :test 'equalp))
         (predicate-indicators (make-hash-table :test #'equalp))
         (relationships))
    (dolist (performative performatives)
      (match performative
        ((ruleml-assert :items items :relationships assert-relationships)
         (unless (null relationships)
           (error "more than one assert in a KB isn't yet supported"))
         (setf relationships assert-relationships)
         (mapc #`(let ((item-predicate-indicators (translate- % prefix-ht prolog-kb-stream)))
                   (setf predicate-indicators
                         (merge-hts predicate-indicators item-predicate-indicators))
                   (format prolog-kb-stream ".~%"))
               items))
        ((ruleml-query :term query-term)
         (format prolog-kb-stream "?- ~A."
                 (translate- query-term prefix-ht prolog-kb-stream)))))
    (values (get-output-stream-string prolog-kb-stream)
            relationships
            predicate-indicators)))

;; TODO: descriptors in transformed atoms are only lists of vars, never, e.g., tuples. fix!
(defun translate- (item prefix-ht stream &optional (assert-item-p t))
  (let ((predicate-indicators (make-hash-table :test #'equalp)))
    (labels ((record-predicate-indicator (name arity &optional recordp)
               (when recordp
                 (sethash (pair name arity) predicate-indicators t)))
             (translate (item &optional stream recordp)
               (match item
                 ((ruleml-oidful-atom
                   :oid oid
                   :predicate (ruleml-atom :root (ruleml-const :contents "Top")
                                           :descriptors (list (ruleml-tuple :dep nil
                                                                            :terms terms))))
                  (record-predicate-indicator "tupterm" (1+ (length terms)) recordp)
                  (format stream "tupterm(~A, ~{~A~^, ~})"
                          (translate oid)
                          (mapcar #'translate terms)))
                 ((ruleml-oidful-atom
                   :oid oid
                   :predicate (ruleml-atom :root root
                                           :descriptors (list (ruleml-tuple :dep t
                                                                            :terms terms))))
                  (record-predicate-indicator "prdtupterm" (+ 2 (length terms)) recordp)
                  (format stream "prdtupterm(~A, ~A, ~{~A~^, ~})"
                          (translate oid)
                          (translate root)
                          (mapcar #'translate terms)))
                 ((ruleml-oidful-atom
                   :oid oid
                   :predicate (ruleml-atom :root (ruleml-const :contents "Top")
                                           :descriptors (list (ruleml-slot :dep nil
                                                                           :name name
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
                  (format stream "prdtupterm(~A, ~A, ~{~A~^, ~})"
                          (translate oid)
                          (translate root)
                          (mapcar #'translate descriptors)))
                 ((ruleml-membership :oid oid :predicate predicate)
                  (record-predicate-indicator "memterm" 2 recordp)
                  (format stream "memterm(~A, ~A)"
                          (translate oid)
                          (translate predicate)))
                 ((or (ruleml-atom :root root :descriptors (list (ruleml-tuple :dep t :terms terms)))
                      (ruleml-expr :root root :terms (list (ruleml-tuple :dep t :terms terms))))
                  (let ((root-string (translate root)))
                    (record-predicate-indicator (format nil "~A" root-string) (length terms) recordp)
                    (if (null terms)
                        (format stream "~A"
                                root-string)
                        (format stream "~A(~{~A~^, ~})"
                                root-string
                                (mapcar #'translate terms)))))
                 ((ruleml-expr :root root :terms terms)
                  (let ((root-string (translate root)))
                    (record-predicate-indicator root-string (length terms) recordp)
                    (format stream "~A(~{~A~^, ~})"
                            root-string
                            (mapcar #'translate terms))))
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
                          (mapcar #'translate terms)))
                 ((ruleml-or :terms terms)
                  (format stream "(~{~A~^; ~})"
                          (mapcar #'translate terms)))
                 ((or (ruleml-exists :formula formula)
                      (ruleml-forall :clause formula))
                  (translate formula stream))
                 ((ruleml-external :atom atom)
                  (translate atom stream))
                 ((ruleml-implies :conclusion conclusion :condition condition)
                  (format stream "~A :- ~A"
                          (translate conclusion nil t)
                          (translate condition)))
                 ((ruleml-var :name name)
                  (format stream "Q~A" name))
                 ((ruleml-naf :formula formula)
                  (format stream "\+ (~A)"
                          (translate formula)))
                 ((ruleml-const :contents const)
                  (match const
                    ((ruleml-pname-ln :name ns :url local)
                     (if ns
                         (multiple-value-bind (url foundp)
                             (gethash ns prefix-ht)
                           (if foundp
                               (cond
                                 ((string-equal "http://www.w3.org/2007/rif-builtin-function#" url)
                                  (if-it (match-builtin-function local)
                                         (format stream "~A" it)
                                         (format stream "~A:~A" ns local)))
                                 ((string-equal "http://www.w3.org/2007/rif-builtin-predicate#" url)
                                  (if-it (match-builtin-predicate local)
                                         (format stream "~A" it)
                                         (format stream "~A:~A" ns local)))
                                 (t (error "~A is not a known prefix" ns)))
                               (format stream "~A:~A" ns local)))
                         (format stream "~A" local)))
                    ((type string)
                     (if (eql (char const 0) #\_)
                         (format stream "'~A'" const)
                         (format stream "'_~A'" const)))
                    ((type number)
                     (format stream "~A" const))))
                 ((ruleml-string :contents const)
                  (format stream "\"~A\"" const)))))
      (translate item stream assert-item-p)
      predicate-indicators)))

(defun match-builtin-function (local)
  (cond
    ((string-equal "numeric-add" local) "'+'")
    ((string-equal "numeric-subtract" local) "'-'")
    ((string-equal "numeric-multiply" local) "'*'")
    ((string-equal "numeric-divide" local) "'/'")
    ((string-equal "numeric-integer-divide" local) "'//'")
    ((string-equal "numeric-mod" local) "rem")))

(defun match-builtin-predicate (local)
  (cond
    ((string-equal "numeric-equal" local) "'=:='")
    ((string-equal "numeric-less-than" local) "'<'")
    ((string-equal "numeric-less-than-or-equal" local) "'=<'")
    ((string-equal "numeric-greater-than" local) "'>'")
    ((string-equal "numeric-greater-than-or-equal" local) "'>='")
    ((string-equal "numeric-not-equal" local) "'=\='")
    ((string-equal "is-literal-integer" local) "integer")))
