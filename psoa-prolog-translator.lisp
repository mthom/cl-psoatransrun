
(in-package #:psoa-prolog-translator)

(named-readtables:in-readtable rutils-readtable)

(defun translate (ruleml-document)
  (let* ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
         (prolog-kb-stream (make-string-output-stream))
         (performatives (ruleml-document-performatives ruleml-document))
         (prefixes (ruleml-document-prefixes ruleml-document))
         (prefix-ht (alist->ht (loop for prefix in prefixes
                                     collect (cons (ruleml-prefix-name prefix)
                                                   (ruleml-prefix-iri-ref prefix)))
                               :test 'equalp)))
    (dolist (performative performatives)
      (match performative
        ((ruleml-assert :items items)
         (mapc #`((translate- % prefix-ht prolog-kb-stream)
                  (format prolog-kb-stream ".~%"))
               items))
        ((ruleml-query :term query-term)
         (format prolog-kb-stream "?- ~A"
                 (translate- query-term prefix-ht prolog-kb-stream)))))
    (get-output-stream-string prolog-kb-stream)))

(defun translate- (item prefix-ht &optional stream)
  (match item
    ((ruleml-oidful-atom :oid oid
                         :predicate (ruleml-atom :root (ruleml-const :contents "Top")
                                                 :descriptors (list (ruleml-tuple :dep nil
                                                                                  :terms terms))))
     (format stream "tupterm(~A, ~{~A~^, ~})"
             (translate- prefix-ht oid)
             (mapcar #`(translate- % prefix-ht) terms)))
    ((ruleml-oidful-atom :oid oid
                         :predicate (ruleml-atom :root root
                                                 :descriptors (list (ruleml-tuple :dep t
                                                                                  :terms terms))))
     (format stream "prdtupterm(~A, ~A, ~{~A~^, ~})"
             (translate- oid prefix-ht)
             (translate- root prefix-ht)
             (mapcar #`(translate- % prefix-ht) terms)))
    ((ruleml-oidful-atom :oid oid
                         :predicate (ruleml-atom :root (ruleml-const :contents "Top")
                                                 :descriptors (list (ruleml-slot :dep nil
                                                                                 :name name
                                                                                 :filler filler))))
     (format stream "sloterm(~A, ~A, ~A)"
             (translate- oid prefix-ht)
             (translate- name prefix-ht)
             (translate- filler prefix-ht)))
    ((ruleml-oidful-atom :oid oid
                         :predicate (ruleml-atom :root root
                                                 :descriptors (list (ruleml-slot :dep t
                                                                                 :name name
                                                                                 :filler filler))))
     (format stream "prdsloterm(~A, ~A, ~A, ~A)"
             (translate- oid prefix-ht)
             (translate- root prefix-ht)
             (translate- name prefix-ht)
             (translate- filler prefix-ht)))
    ((ruleml-oidful-atom :oid oid :predicate (ruleml-atom :root root :descriptors descriptors))
     (format stream "prdtupterm(~A, ~A, ~{~A~^, ~})"
             (translate- oid prefix-ht)
             (translate- root prefix-ht)
             (mapcar #`(translate- % prefix-ht) descriptors)))
    ((ruleml-membership :oid oid :predicate predicate)
     (format stream "memterm(~A, ~A)"
             (translate- oid prefix-ht)
             (translate- predicate prefix-ht)))
    ((or (ruleml-atom :root root :descriptors (list (ruleml-tuple :dep t :terms terms)))
         (ruleml-expr :root root :terms (list (ruleml-tuple :dep t :terms terms))))
     (if (null terms)
         (format stream "~A"
                 (translate- root prefix-ht))
         (format stream "~A(~{~A~^, ~})"
                 (translate- root prefix-ht)
                 (mapcar #`(translate- % prefix-ht) terms))))
    ((ruleml-expr :root root :terms terms)
     (format stream "~A(~{~A~^, ~})"
             (translate- root prefix-ht)
             (mapcar #`(translate- % prefix-ht) terms)))
    ((ruleml-equal :left left :right (ruleml-external :atom atom))
     (format stream "is(~A, ~A)"
             (translate- left prefix-ht)
             (translate- atom prefix-ht)))
    ((ruleml-equal :left left :right right)
     (format stream "'='(~A, ~A)"
             (translate- left prefix-ht)
             (translate- right prefix-ht)))
    ((ruleml-and :terms terms)
     (format stream "(~{~A~^, ~})"
             (mapcar #`(translate- % prefix-ht) terms)))
    ((ruleml-or :terms terms)
     (format stream "(~{~A~^; ~})"
             (mapcar #`(translate- % prefix-ht) terms)))
    ((or (ruleml-exists :formula formula)
         (ruleml-forall :clause formula))
     (translate- formula prefix-ht stream))
    ((ruleml-external :atom atom)
     (translate- atom prefix-ht stream))
    ((ruleml-implies :conclusion conclusion :condition condition)
     (format stream "~A :- ~A"
             (translate- conclusion prefix-ht)
             (translate- condition prefix-ht)))
    ((ruleml-var :name name)
     (format stream "?Q~A" name))
    ((ruleml-naf :formula formula)
     (format stream "\+ (~A)" (translate- formula prefix-ht)))
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
     (format stream "\"~A\"" const))))

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
