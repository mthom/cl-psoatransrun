
(in-package #:psoa-prolog-translator)

(named-readtables:in-readtable rutils-readtable)

(defun translate-query (query prefix-ht)
  (let* ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
         (query-vars)
         (output-stream (make-string-output-stream)))
    (transform-ast (ruleml-query-term query)
                   (lambda (term &key &allow-other-keys)
                     (when (ruleml-genvar-p term)
                       (pushnew (ruleml-var-name term) query-vars))
                     term))
    (-translate (ruleml-query-term query)
                prefix-ht
                output-stream
                nil)
    (values (format nil "~A." (get-output-stream-string output-stream))
            (format nil "[~{Q~A~^, ~}]." query-vars))))

(defun sort-kb (prolog-kb-string predicate-indicators)
  (let* ((stream (make-string-input-stream prolog-kb-string))
         (lines (loop for line = (read-line stream nil)
                      for pred-ind in predicate-indicators
                      if (and line pred-ind)
                        collect (cons line pred-ind))))
    (remove-duplicates
     (mapcar #'car
             (sort lines (lambda (p1 p2)
                           ;; Sort lexicographically by predicate indicator.
                           (or (string< (car p1) (car p2))
                               (and (<= (cdr p1) (cdr p2))
                                    (string= (car p1) (car p2)))))
                   :key #'cdr))
     :test #'equal)))

(defun translate-document (document &key (system :scryer))
  (multiple-value-bind (prolog-kb-string relationships
                        is-relational-p predicate-indicators)
      (translate-document- document)
    (ecase system
      (:scryer (let ((collated-stream (make-string-output-stream))
                     (lines (sort-kb prolog-kb-string predicate-indicators))
                     (predicate-indicators (alist->ht (mapcar #`(cons % t) predicate-indicators)
                                                      :test #'equalp)))
                 (format collated-stream ":- use_module(library(tabling)).~%~%")
                 (loop for key being each hash-key of predicate-indicators
                       if (and (car key) (cdr key))
                       do (format collated-stream ":- table ~A/~A.~%"
                                  (car key) (cdr key)))
                 (format collated-stream "~%")
                 (dolist (line lines)
                   (format collated-stream "~A~%" line))
                   (values (get-output-stream-string collated-stream)
                           relationships
                           is-relational-p))))))

(defun translate-document- (document)
  (let* ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
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
         (unless (null relationships)
           (error "multiple Assert's in a single PSOA KB isn't yet supported"))
         (setf relationships assert-relationships
               is-relational-p assert-is-relational-p)
         (dolist (item items)
           (let ((item-predicate-indicator (-translate item prefix-ht prolog-kb-stream)))
             (push item-predicate-indicator predicate-indicators)
             (format prolog-kb-stream ".~%"))))
        ((ruleml-query :term query-term)
         (format prolog-kb-stream "?- ~A."
                 (-translate query-term prefix-ht prolog-kb-stream)))))
    (values (get-output-stream-string prolog-kb-stream)
            relationships
            is-relational-p
            (nreverse predicate-indicators))))

(defun -translate (item prefix-ht stream &optional (assert-item-p t))
  (let (predicate-indicators)
    (macrolet ((record-predicate-indicator (name arity recordp)
                 `(when ,recordp
                    (setf predicate-indicators (cons ,name ,arity)))))
      (labels ((translate (item &optional stream recordp)
                 (match item
                   ((ruleml-oidful-atom
                     :oid oid
                     :predicate (ruleml-atom :root (ruleml-const :contents "Top")
                                             :descriptors (list (ruleml-tuple :dep nil
                                                                              :terms terms))))
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
                       (record-predicate-indicator
                        (make-url-const ns local prefix-ht)
                        0
                        recordp)
                       (make-url-const ns local prefix-ht stream))
                      ((type string)
                       (record-predicate-indicator const 0 recordp)
                       (if (eql (char const 0) #\_)
                           (format stream "'~A'" const)
                           (format stream "'_~A'" const)))
                      ((type number)
                       (format stream "~A" const))))
                   ((ruleml-number :value number)
                    (format stream "~A" number))
                   ((ruleml-string :contents const)
                    (format stream "\"~A\"" const)))))
        (translate item stream assert-item-p)
        predicate-indicators))))
