
(in-package #:psoa-transformers)

(named-readtables:in-readtable rutils-readtable)

(defun fresh-variable (&optional (prefix "Var"))
  (make-ruleml-var :name (format nil "~A" (gensym prefix))))

(defun fresh-constant (&optional (prefix "_"))
  (make-ruleml-const :contents (string-downcase (format nil "~A" (gensym prefix)))))

(defun ground-atom-p (atom)
  (let ((is-ground t))
    (walk-ast atom
              (lambda (term)
                (when (ruleml-var-p term)
                  (setf is-ground nil))))
    is-ground))

#|
(walk-ast *
          (lambda (term)
             (match term
              ((ruleml-assert :items items)
               (make-ruleml-assert :items (mapcar #'unnest items)))
              (_ term)))
          #'identity)
|#

(defun map-atom-transformer (transformer term)
  (match term
    ((ruleml-implies :conclusion conclusion :condition condition)
     (make-ruleml-implies :conclusion (map-atom-transformer transformer conclusion)
                          :condition (map-atom-transformer transformer condition)))
    ((ruleml-forall :vars vars :clause clause)
     (make-ruleml-forall :vars vars
                         :clause (map-atom-transformer transformer clause)))
    ((ruleml-exists :vars vars :formula formula)
     (make-ruleml-exists :vars vars
                         :formula (map-atom-transformer transformer formula)))
    ((ruleml-and :terms terms)
     (make-ruleml-and :terms (mapcar #`(map-atom-transformer transformer %) terms)))
    ((ruleml-or :terms terms)
     (make-ruleml-or :terms (mapcar #`(map-atom-transformer transformer %) terms)))
    ((ruleml-naf :formula formula)
     (make-ruleml-naf :formula (map-atom-transformer transformer formula)))
    ((or (ruleml-oidful-atom) (ruleml-membership) (ruleml-atom) (ruleml-expr))
     (funcall transformer term))
    (_ term)))

(defun embedded-objectify (term)
  (map-atom-transformer #'-embedded-objectify term))

(defun -embedded-objectify (atom)
  (let (*in-psoa-rest*
        embedded-oids)
    (declare (special *in-psoa-rest*))
    (labels ((substep (term)
               (let ((*in-psoa-rest* t))
                 (declare (special *in-psoa-rest*))
                 (walker term)))
             (walker (term)
               (match term
                 ((ruleml-atom :root root :descriptors descriptors)
                  (if *in-psoa-rest*
                      (let ((oid (if (ground-atom-p term)
                                     (fresh-constant)
                                     (let ((var (fresh-variable)))
                                       (push var embedded-oids)
                                       var))))
                        (make-ruleml-oidful-atom
                         :oid oid
                         :predicate
                         #1=(make-ruleml-atom :root (walker root)
                                              :descriptors (mapcar #`(walker %) descriptors))))
                      (let ((*in-psoa-rest* t))
                        (declare (special *in-psoa-rest*))
                        #1#)))
                 ((ruleml-oidful-atom :oid oid
                                      :predicate (ruleml-atom :root root
                                                              :descriptors descriptors))
                  (make-ruleml-oidful-atom :oid (substep oid)
                                           :predicate #1#))
                 ((ruleml-membership :oid oid
                                     :predicate (ruleml-atom :root root
                                                             :descriptors descriptors))
                  (make-ruleml-membership :oid (substep oid)
                                          :predicate #1#))
                 ((ruleml-slot :dep dep :name name :filler filler)
                  (make-ruleml-slot :dep dep :name (substep name) :filler (substep filler)))
                 ((ruleml-tuple :dep dep :terms terms)
                  (make-ruleml-tuple :dep dep :terms (mapcar #`(substep %) terms)))
                 ((ruleml-expr :root root :terms terms)
                  (make-ruleml-expr :root root :terms (mapcar #`(substep %) terms)))
                 (_
                  term))))
      (let ((atom (walker atom)))
        (if (null embedded-oids)
            atom
            (make-ruleml-exists :vars embedded-oids
                                :formula atom))))))

(defun unnest (term)
  (map-atom-transformer #'-unnest term))

(defun -unnest (term)
  (let ((trimmed-terms))
    (labels ((check-oid (oid)
               (match oid
                 ((ruleml-var :name name)
                  (if (null name)
                      (make-ruleml-var :name (fresh-variable))
                      oid))
                 ("_" (fresh-constant))
                 (_ oid)))
             (atoms (term)
               (match term
                 ((or (ruleml-oidful-atom :oid oid :predicate atom)
                      (ruleml-membership :oid oid :predicate atom))
                  (push (trim term) trimmed-terms)
                  (atoms oid)
                  (atoms atom))
                 ((ruleml-atom :root root :descriptors descriptors)
                  (unless trimmed-terms
                    (push (trim term) trimmed-terms))
                  (atoms root)
                  (mapc #'atoms descriptors))
                 ((ruleml-expr :root root :terms terms)
                  (atoms root)
                  (mapc #'atoms terms))
                 ((ruleml-tuple :terms terms)
                  (mapc #'atoms terms))
                 ((ruleml-slot :name name :filler filler)
                  (atoms name)
                  (atoms filler))))
             (trim (term)
               (match term
                 ((or (ruleml-oidful-atom :oid oid :predicate atom :position pos)
                      (ruleml-membership :oid oid :predicate atom :position pos))
                  (make-ruleml-oidful-atom :oid (check-oid (retain oid))
                                           :predicate (retain atom)
                                           :position pos))
                 ((ruleml-atom :root root :descriptors descriptors :position pos)
                  (make-ruleml-atom :root (retain root)
                                    :descriptors (mapcar #'retain descriptors)
                                    :position pos))
                 ((ruleml-expr :root root :terms terms :position pos)
                  (make-ruleml-expr :root (retain root)
                                    :terms (mapcar #'retain terms)
                                    :position pos))
                 ((ruleml-slot :dep dep :name name :filler filler :position pos)
                  (make-ruleml-slot :dep dep
                                    :name (retain name)
                                    :filler (retain filler)
                                    :position pos))
                 ((ruleml-tuple :dep dep :terms terms :position pos)
                  (make-ruleml-tuple :dep dep
                                     :terms (mapcar #'retain terms)
                                     :position pos))
                 (_ term)))
             (retain (term)
               (match term
                 ((or (ruleml-oidful-atom :oid oid) (ruleml-membership :oid oid))
                  (retain oid))
                 ((or (ruleml-atom) (ruleml-slot) (ruleml-expr) (ruleml-tuple))
                  (trim term))
                 (_ term))))
      (atoms term)
      (cond ((null trimmed-terms) term)
            ((single trimmed-terms) (first trimmed-terms))
            (t (make-ruleml-and :terms trimmed-terms))))))

#|
   Differentiated static objectification, on an atom \omega:

   1) If \omega is oidful, then objectify(\omega) = \omega.

   2) If \omega is a non-ground fact, or a rule conclusion atom, or a query atom,
      then objectify(\omega) = Exists ?j (?j#f(...)).

   3) If \omega is a rule condition atom, objectify(\omega) = ?j#f(...) where ?j
      is a fresh variable scoped universally by the enclosing rule.
|#

(defun objectify-static-diff (term)
  (let (vars)
   (labels ((forall-clause (clause)
              (if (or (null vars) (ruleml-forall-p clause))
                  clause
                  (make-ruleml-forall :vars vars :clause clause)))
            (exists-fn (atom) ;; Rule 2.
              (let ((oid (fresh-variable)))
                (make-ruleml-exists :vars (list oid)
                                    :formula (make-ruleml-oidful-atom :oid oid
                                                                      :predicate atom))))
            (oid-fn (atom) ;; Rule 3.
              (let ((oid (fresh-variable)))
                (push oid vars)
                (make-ruleml-oidful-atom :oid oid :predicate atom)))
            (query-fn (term)
              (match term
                ((ruleml-and :terms terms)
                 (make-ruleml-and :terms (mapcar #'query-fn terms)))
                ((ruleml-or :terms terms)
                 (make-ruleml-or :terms (mapcar #'query-fn terms)))
                ((ruleml-atom)
                 (exists-fn term))
                (_ term)))
            (loop-fn (term subst-fn)
              (match term
                ((ruleml-atom)
                 (if (ground-atom-p term)
                     term
                     (funcall subst-fn term)))
                ((ruleml-implies :conclusion conclusion :condition condition)
                 (forall-clause (make-ruleml-implies :conclusion (loop-fn conclusion #'exists-fn)
                                                     :condition  (loop-fn condition #'oid-fn))))
                ((ruleml-forall :vars clause-vars :clause clause)
                 (setf vars (copy-list clause-vars))
                 (forall-clause (loop-fn clause subst-fn)))
                ((ruleml-query :term query)
                 (make-ruleml-query :term (query-fn query)))
                ((ruleml-and :terms terms)
                 (make-ruleml-and :terms (mapcar (lambda (term) (loop-fn term subst-fn)) terms)))
                ((ruleml-or :terms terms)
                 (make-ruleml-or :terms (mapcar (lambda (term) (loop-fn term subst-fn)) terms)))
                (_ term))))
     (loop-fn term #'exists-fn))))


#| Dynamic objectification:

   1) If \omega is a relationship, objectify_d(\phi, \omega) = \omega.

   2) If \omega has a non-variable OID or a slot, objectify_d(\phi, \omega) = Or().

   3) If \omega has an OID variable and m > 0 tuples, being of the form ;
      ?O#f([t_{1,1}, \ldots, t_{1,n_1}], \ldots, [t_{m,1}, \ldots, t_{m,n_m}]),
      then tupribution happens, and we end up with m disjunctions of the form:

      And(\ldots
          f(t_{1,i} \ldots t_{i,n}) ?O = _oidcons(f t_{i,1} \ldots t_{i,n})
         )

   4) If \omega is a membership of the form ?O#f, objectify_d(\phi, \omega) is a disjunction
      of k formulas objectify_d(?O#f(?X_1 \ldots ?X_{n_i})), where $n_1, \ldots, n_k$ are the k
      different arities of f in the KB:

      Or(objectify_d(\ldots ?O#f(?X_1 \ldots ?X_{n_i})) \ldots)

   5) If \omega is of the form f(...) has no OID but m tuples, m > 1,
      objectify_d(\phi, \omega) = Exists ?O objectify_d(?O#f(...))
      where ?O is a fresh variable in the query.
|#

(defun make-oid-cons (root terms)
  (make-ruleml-atom :root "_oid_cons" :descriptors (cons root terms)))

(defun atom-has-slot-p (atom)
  (some #'ruleml-slot-p (ruleml-atom-descriptors atom)))

(defun is-relationship-p (atom)
  (let ((descriptors (ruleml-atom-descriptors atom)))
    (and (single descriptors) (not (atom-has-slot-p atom)))))

(defun objectify-dynamic (term &optional (relationships (make-hash-table :test #'string=)))
  (match term
    ((ruleml-oidful-atom :oid oid :predicate atom)
     (cond ((or (not (ruleml-var-p oid)) (atom-has-slot-p atom)) (make-ruleml-or)) ;; 2.
           (t ;; 3
            (make-ruleml-and :terms
                             (loop with root  = (ruleml-atom-root atom)
                                   for tuple in (ruleml-atom-descriptors atom)
                                   for terms = (ruleml-tuple-terms tuple)
                                   for atom  = (make-ruleml-atom :root root :descriptors terms)
                                   for oid-cons-equal = (make-ruleml-equal
                                                         :left  oid
                                                         :right (make-oid-cons root terms))
                                   nconc (list atom oid-cons-equal))))))
    ((ruleml-atom :descriptors descriptors)
     (cond ((atom-has-slot-p term) (make-ruleml-or)) ;; 2.
           ((single descriptors) term) ;; 1.
           (t (let ((oid (fresh-variable))) ;; 5.
                (make-ruleml-exists
                 :vars (list oid)
                 :formula (objectify-dynamic
                           (make-ruleml-oidful-atom :oid oid :predicate term)
                           relationships))))))
    ((ruleml-membership :oid oid :predicate predicate)
     (if (ruleml-var-p oid)
         (multiple-value-bind (tuple-arities found) ;; 4.
             (gethash predicate relationships)
           (if found
               (make-ruleml-or
                :terms
                (mapcar (lambda (k)
                          (let ((tuple (loop repeat k collect (fresh-variable))))
                            (objectify-dynamic
                             (make-ruleml-oidful-atom
                              :oid oid
                              :predicate (make-ruleml-atom :root predicate
                                                           :descriptors tuple))
                             relationships)))
                        tuple-arities))
               term))
         (make-ruleml-or))) ;; 2.
    (_
     term)))

#|
Static/dynamic objectification:

1) Is objectify_s(\phi, \omega) if \omega is a non-relational atom, or,
is objectify_d(\phi, \omega) if \omega is relational.

|#

(defun predicate-name (atom)
  (match atom
    ((ruleml-membership :predicate predicate)
     predicate)
    ((ruleml-atom :root root)
     root)
    ((ruleml-oidful-atom :predicate atom)
     (predicate-name atom))
    ((ruleml-implies :conclusion head)
     (predicate-name head))
    ((ruleml-forall :clause clause)
     (predicate-name clause))))

(defun objectify (term relationships)
  (labels ((dispatch (term predicate)
             (multiple-value-bind (tuple-arities found)
                 (gethash predicate relationships)
               (declare (ignore tuple-arities))
               (if found
                   (objectify-dynamic term relationships)
                   (objectify-static-diff term))))
           (loop-fn (inner-term)
             (match inner-term
               ((or (ruleml-oidful-atom) (ruleml-atom)
                    (ruleml-implies) (ruleml-forall))
                (dispatch inner-term (predicate-name inner-term)))
               ((ruleml-membership :predicate predicate)
                (dispatch inner-term predicate))
               ((ruleml-query :term query)
                (make-ruleml-query :term (loop-fn query)))
               ((ruleml-and :terms terms)
                (make-ruleml-and :terms (mapcar #'loop-fn terms)))
               ((ruleml-or :terms terms)
                (make-ruleml-or :terms (mapcar #'loop-fn terms)))
               (_ term))))
    (loop-fn term)))

(defun kb-relationships (ruleml-assert)
  (let ((relationships (make-hash-table :test #'equal))
        (blacklist))
    (labels ((consider-atom (atom)
               (if (is-relationship-p atom)
                   (push (length (ruleml-tuple-terms (first (ruleml-atom-descriptors atom))))
                         (gethash (ruleml-atom-root atom) relationships))
                   (push (ruleml-atom-root atom) blacklist)))
             (consider-assert-item (item)
               (match item
                 ((ruleml-atom)
                  (consider-atom item))
                 ((ruleml-implies :conclusion (ruleml-atom))
                  (consider-atom (ruleml-implies-conclusion item)))
                 ((ruleml-forall :clause clause)
                  (consider-assert-item clause)))))
      (walk-ast ruleml-assert #'identity #'consider-assert-item)
      (dolist (root blacklist)
        (remhash root relationships))
      relationships)))

(defun subclass-rewrite (term)
  (match term
    ((ruleml-subclass-rel :super super :sub sub)
     (let* ((var   (fresh-variable))
            (head  (make-ruleml-oidful-atom :oid var :predicate super))
            (body  (make-ruleml-oidful-atom :oid var :predicate sub)))
        (make-ruleml-forall :vars (list var)
                            :clause (make-ruleml-implies :conclusion head
                                                         :condition  body))))
    ((ruleml-forall :vars vars :clause (ruleml-subclass-rel :super super :sub sub))
     (let* ((var   (fresh-variable))
            (head  (make-ruleml-oidful-atom :oid var :predicate super))
            (body  (make-ruleml-oidful-atom :oid var :predicate sub)))
        (make-ruleml-forall :vars (cons var vars)
                            :clause (make-ruleml-implies :conclusion head
                                                         :condition  body))))
    (_ term)))

(defun transform (document)
  (make-ruleml-document
   :base (ruleml-document-base document)
   :prefixes (ruleml-document-prefixes document)
   :imports (ruleml-document-imports document)
   :performatives
   (mapcar (lambda (term)
             (match term
               ((ruleml-assert :items items)
                (let ((relationships (kb-relationships term)))
                  (make-ruleml-assert
                   :items (mapcar #`(objectify (subclass-rewrite (unnest (embedded-objectify %)))
                                               relationships)
                                  items))))
               ((ruleml-query :term query)
                (let ((relationships (make-hash-table :test #'equal)))
                  (make-ruleml-query :term (objectify (unnest query) relationships))))
               (_ term)))
           (ruleml-document-performatives document))))
