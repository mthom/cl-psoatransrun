
(in-package #:psoa-transformers)

(named-readtables:in-readtable rutils-readtable)

(defun fresh-variable (&optional (prefix "Var"))
  (make-ruleml-genvar :name (format nil "~A" (gensym prefix))))

(defun fresh-constant (&optional (prefix "_"))
  (make-ruleml-const :contents (string-downcase (format nil "~A" (gensym prefix)))))

(defun fresh-skolem-constant ()
  (make-ruleml-const :contents (string-downcase (format nil "skolem~A" (gensym "")))))

(defun ground-atom-p (atom)
  (transform-ast atom
                 (lambda (term &key &allow-other-keys)
                   (when (ruleml-var-p term)
                     (return-from ground-atom-p nil))))
  t)

(defun make-ruleml-oidful (oid predicate)
  (if (ruleml-const-p predicate)
      (make-ruleml-membership :oid oid :predicate predicate)
      (make-ruleml-oidful-atom :oid oid :predicate predicate)))

(defun subclass-rewrite (term)
  (match term
    ((ruleml-subclass-rel :super super :sub sub)
     (let* ((var   (fresh-variable))
            (head  (make-ruleml-oidful var super))
            (body  (make-ruleml-oidful var sub)))
       (make-ruleml-forall :vars (list var)
                           :clause (make-ruleml-implies :conclusion head
                                                        :condition  body))))
    ((ruleml-forall :vars vars :clause (ruleml-subclass-rel :super super :sub sub))
     (let* ((var   (fresh-variable))
            (head  (make-ruleml-oidful var super))
            (body  (make-ruleml-oidful var sub)))
       (make-ruleml-forall :vars (cons var vars)
                           :clause (make-ruleml-implies :conclusion head
                                                        :condition  body))))
    (_ term)))


(defun map-atom-transformer (transformer term &rest args &key &allow-other-keys)
  (match term
    ((or (ruleml-oidful-atom) (ruleml-membership) (ruleml-atom) (ruleml-expr))
     (apply transformer term args))
    (_ (apply #'transform-ast term transformer
              :propagator (lambda (term &rest args &key &allow-other-keys)
                            (apply #'map-atom-transformer transformer term args))
              args))))


(defun embedded-objectify (term)
  (if (and (not psoatransrun::*static-objectification-only*)
           psoatransrun::*is-relational-p*)
      term
      (map-atom-transformer #'-embedded-objectify term)))

(defun -embedded-objectify (atom &key positive negative &allow-other-keys)
  (let (in-psoa-rest-p
        embedded-oids
        (ground-atom-p (ground-atom-p atom)))
    (declare (special in-psoa-rest-p))
    (labels ((substep (term &key &allow-other-keys)
               (let ((in-psoa-rest-p t))
                 (declare (special in-psoa-rest-p))
                 (walker term)))
             (walker (term &key &allow-other-keys)
               (match term
                 ((ruleml-atom :root root :descriptors descriptors)
                  (if in-psoa-rest-p
                      (let ((oid (if (and (not positive) (not negative) ground-atom-p)
                                     (fresh-constant)
                                     (let ((var (fresh-variable)))
                                       (push var embedded-oids)
                                       var))))
                        (make-ruleml-oidful-atom
                         :oid oid
                         :predicate
                         #1=(make-ruleml-atom :root (walker root)
                                              :descriptors (mapcar #`(walker %) descriptors))))
                      (let ((in-psoa-rest-p t))
                        (declare (special in-psoa-rest-p))
                        #1#)))
                 ((ruleml-oidful-atom :oid oid :predicate predicate)
                  (make-ruleml-oidful
                   (substep oid)
                   (transform-ast predicate
                                  (lambda (term &key &allow-other-keys) term)
                                  :propagator #'substep)))
                 ((ruleml-membership :oid oid :predicate predicate)
                  (make-ruleml-oidful
                   (substep oid)
                   (transform-ast predicate
                                  (lambda (term &key &allow-other-keys) term)
                                  :propagator #'substep)))
                 (_ (transform-ast
                     term
                     (lambda (term &key &allow-other-keys) term)
                     :propagator #'walker)))))
      (let ((atom (walker atom)))
        (if (null embedded-oids)
            atom
            (make-ruleml-exists :vars embedded-oids :formula atom))))))


(defun unnest (term)
  (map-atom-transformer #'-unnest term))

(defun -unnest (term &key &allow-other-keys)
  (let (trimmed-terms)
    (labels ((check-oid (oid)
               (match oid
                 ((ruleml-var :name name)
                  (if (string= name "")
                      (fresh-variable)
                      oid))
                 ("_" (fresh-constant))
                 (_ oid)))
             (atoms (term &key &allow-other-keys)
               (match term
                 ((or (ruleml-oidful-atom) (ruleml-membership))
                  (push (trim term) trimmed-terms)))
               term)
             (trim (term)
               (match term
                 ((ruleml-membership :oid oid :predicate atom :position pos)
                  (make-ruleml-membership :oid (check-oid (retain oid))
                                          :predicate (retain atom)
                                          :position pos))
                 ((ruleml-oidful-atom :oid oid :predicate atom :position pos)
                  (make-ruleml-oidful-atom :oid (check-oid (retain oid))
                                           :predicate (retain atom)
                                           :position pos))
                 ((or (ruleml-atom) (ruleml-expr) (ruleml-slot) (ruleml-tuple))
                  (transform-ast term
                                 (lambda (term &key &allow-other-keys) term)
                                 :propagator (lambda (term &key &allow-other-keys)
                                               (retain term))))
                 (_ term)))
             (retain (term)
               (match term
                 ((or (ruleml-oidful-atom :oid oid) (ruleml-membership :oid oid))
                  (retain oid))
                 ((or (ruleml-atom) (ruleml-expr) (ruleml-slot) (ruleml-tuple))
                  (trim term))
                 (_ term))))
      (match term
        ((or (ruleml-oidful-atom) (ruleml-atom) (ruleml-membership) (ruleml-expr))
         (transform-ast term #'atoms)
         (when (ruleml-atom-p term)
           (push (trim term) trimmed-terms))))
      (cond ((null trimmed-terms) term)
            ((single trimmed-terms) (first trimmed-terms))
            (t (make-ruleml-and :terms (nreverse trimmed-terms)))))))


#|
Differentiated static objectification, on an atom \omega:

1) If \omega is a ground fact, objectify(\omega) = _i#f(...) where _i
is a fresh local constant symbol which neithers occurs elsewhere in KB(\omega)
nor is used for the objectification of other atoms.

2) If \omega is a non-ground fact, or a rule conclusion atom, or a query atom,
then objectify(\omega) = Exists ?j (?j#f(...)).

3) If \omega is a rule condition atom, objectify(\omega) = ?j#f(...) where ?j
is a fresh variable scoped universally by the enclosing rule.
|#

(defun objectify-static (term &key positive negative &allow-other-keys)
  (let (vars)
    (values
     (match term
       ((or (ruleml-atom) (ruleml-const))
        (let ((ground-atom-p (ground-atom-p term)))
          (cond ((and ground-atom-p (not positive) (not negative)) ; 1
                 (make-ruleml-oidful (fresh-constant)
                                     (if (ruleml-const-p term)
                                         (make-ruleml-atom :root term)
                                         term)))
                ((or (not (or ground-atom-p positive negative))
                     (and positive negative)
                     positive)         ; 2
                 (let ((var (fresh-variable)))
                   (make-ruleml-exists
                    :vars (list var)
                    :formula (make-ruleml-oidful var term))))
                (t (let ((var (fresh-variable))) ; 3
                     (push var vars)
                     (make-ruleml-oidful var term))))))
       (_ term))
     vars)))

#| Dynamic objectification:

1) If \omega is in a KB atom, objectify_d(\omega) = \omega.

2) If \omega is a query atom, there are five subcases:

2.1) If \omega is a relationship, objectify_d(\phi, \omega) = \omega.
                                        ; ; ; ;
2.2) If \omega has a non-variable OID or a slot, objectify_d(\phi, \omega) = Or(). ; ;

2.3) If \omega has an OID variable and m > 0 tuples, being of the form ;
?O#f([t_{1,1}, \ldots, t_{1,n_1}], \ldots, [t_{m,1}, \ldots, t_{m,n_m}]),
then tupribution happens, and we end up with m disjunctions of the form:

And(\ldots
f(t_{1,i} \ldots t_{i,n}) ?O = _oidcons(f t_{i,1} \ldots t_{i,n})
)

2.4) If \omega is a membership of the form ?O#f(), objectify_d(\phi,
\omega) is a disjunction of k formulas objectify_d(?O#f(?X_1 \ldots
?X_{n_i})), where $n_1, \ldots, n_k$ are the k different arities of f
in the KB:

Or(objectify_d(\ldots ?O#f(?X_1 \ldots ?X_{n_i})) \ldots)

2.5) If \omega is of the form f(...) has no OID but m tuples, m > 1,
objectify_d(\phi, \omega) = Exists ?O objectify_d(?O#f(...))
where ?O is a fresh variable in the query.

|#

(defun make-oid-cons (root terms)
  (make-ruleml-atom :root (make-ruleml-const :contents "_oid_cons")
                    :descriptors (list (make-ruleml-tuple :dep t :terms (cons root terms)))))

(defun is-relationship-p (term prefix-ht)
  (if-it (predicate-name term prefix-ht)
         (transform-ast term
                  (lambda (term &key &allow-other-keys)
                    (match term
                      ((ruleml-atom :descriptors descriptors)
                       (when (and (string= it (predicate-name term prefix-ht))
                                  (not (and (single descriptors)
                                            (ruleml-tuple-p (first descriptors))
                                            (ruleml-tuple-dep (first descriptors)))))
                         (return-from is-relationship-p nil))
                       term)
                      ((or (ruleml-oidful-atom) (ruleml-membership))
                       (when (string= it (predicate-name term prefix-ht))
                         (return-from is-relationship-p nil))
                       term)
                      (_ term)))))
  t)

(defun objectify-dynamic (term relationships prefix-ht &key positive negative &allow-other-keys)
  (if (and positive negative)
      ;; 2
      (match term
        ((ruleml-atom :root root :descriptors descriptors)
         (cond ((when-it (gethash root relationships)
                  (and (is-relationship-p term prefix-ht)
                       (member (length (ruleml-tuple-terms (first descriptors)))
                               it)))
                term) ;; 2.1
               ((some (lambda (term)
                        (or (ruleml-slot-p term)
                            (not (ruleml-tuple-dep term))))
                      descriptors)
                (make-ruleml-or))               ;; 2.2
               (t (let ((oid (fresh-variable))) ;; 2.5
                    (make-ruleml-exists
                     :vars (list oid)
                     :formula (objectify-dynamic
                               (make-ruleml-oidful-atom :oid oid :predicate term)
                               relationships
                               prefix-ht
                               :positive positive
                               :negative negative))))))
        ((ruleml-oidful-atom :oid oid :predicate (ruleml-atom :root root :descriptors descriptors))
         (cond ((or (not (ruleml-var-p oid)) ;; 2.2
                    (some (lambda (term)
                            (or (ruleml-slot-p term)
                                (not (ruleml-tuple-dep term))))
                          descriptors))
                (make-ruleml-or))
               (t ;; 2.3
                (make-ruleml-and :terms
                                 (loop for tuple in descriptors
                                       for terms = (ruleml-tuple-terms tuple)
                                       for atom  = (make-ruleml-atom :root root :descriptors (list tuple))
                                       for oid-cons-equal = (make-ruleml-equal
                                                             :left  oid
                                                             :right (make-oid-cons root terms))
                                       nconc (list atom oid-cons-equal))))))
        ((ruleml-membership :oid oid :predicate predicate)
         (if (ruleml-var-p oid)
             (multiple-value-bind (tuple-arities foundp) ;; 2.4
                 (gethash (predicate-name predicate prefix-ht) relationships)
               (if foundp
                   (make-ruleml-or
                    :terms
                    (mapcar (lambda (k)
                              (let ((tuple (make-ruleml-tuple
                                            :dep t
                                            :terms (loop repeat k collect (fresh-variable)))))
                                (objectify-dynamic
                                 (make-ruleml-oidful-atom
                                  :oid oid
                                  :predicate (make-ruleml-atom :root predicate
                                                               :descriptors (list tuple)))
                                 relationships
                                 prefix-ht
                                 :positive positive
                                 :negative negative)))
                            tuple-arities))
                   term))
             (make-ruleml-or)))
        (_ term))
      term)) ;; 1

#|
Static/dynamic objectification:

1) Is objectify_s(\phi, \omega) if \omega is a non-relational atom, or,
is objectify_d(\phi, \omega) if \omega is relational.

|#

(defun kb-relationships (ruleml-assert prefix-ht)
  (let ((relationships (make-hash-table :test #'equal))
        (blacklist))
    (labels ((consider-atom (term head-atom)
               (if (is-relationship-p term prefix-ht)
                   (pushnew (length (ruleml-tuple-terms (first (ruleml-atom-descriptors head-atom))))
                            (gethash (predicate-name head-atom prefix-ht) relationships))
                   (push (predicate-name head-atom prefix-ht)
                         blacklist)))
             (consider-assert-item (item &key &allow-other-keys)
               (match item
                 ((or (ruleml-oidful-atom) (ruleml-atom))
                  (consider-atom item item))
                 ((ruleml-implies :conclusion (ruleml-atom))
                  (consider-atom item (ruleml-implies-conclusion item)))
                 ((ruleml-forall :clause clause)
                  (consider-assert-item clause)))))
      (transform-ast ruleml-assert
                     (lambda (term &key &allow-other-keys) term)
                     :propagator #'consider-assert-item)
      (dolist (root blacklist)
        (remhash root relationships))
      ;; return values: relationship hash table, boolean that is t iff
      ;; the Assert is completely relational.
      (values relationships (null blacklist)))))


(defun match-builtin-function (local)
  (match local
    ("numeric-add" "'+'")
    ("numeric-subtract" "'-'")
    ("numeric-multiply" "'*'")
    ("numeric-divide" "'/'")
    ("numeric-integer-divide" "'//'")
    ("numeric-mod" "rem")))

(defun match-builtin-predicate (local)
  (match local
    ("numeric-equal" "'=:='")
    ("numeric-less-than" "'<'")
    ("numeric-less-than-or-equal" "'=<'")
    ("numeric-greater-than" "'>'")
    ("numeric-greater-than-or-equal" "'>='")
    ("numeric-not-equal" "'=\='")
    ("is-literal-integer" "integer")))

(defun make-url-const (ns local prefix-ht &optional stream)
  (multiple-value-bind (url foundp)
      (gethash ns prefix-ht)
    (if foundp
        (match url
          ("http://www.w3.org/2007/rif-builtin-function#"
           (if-it (match-builtin-function local)
                  (format stream "~A" it)
                  (format stream "'<~A~A>'" url local)))
          ("http://www.w3.org/2007/rif-builtin-predicate#"
           (if-it (match-builtin-predicate local)
                  (format stream "~A" it)
                  (format stream "'<~A~A>'" url local)))
          (_ (format stream "'<~A~A>'" url local)))
        (format stream "'<~A~A>'" ns local))))

(defun predicate-name (atom prefix-ht)
  (match atom
    ((ruleml-membership :predicate predicate)
     (predicate-name predicate prefix-ht))
    ((ruleml-atom :root root)
     (predicate-name root prefix-ht))
    ((ruleml-oidful-atom :predicate atom)
     (predicate-name atom prefix-ht))
    ((ruleml-implies :conclusion head)
     (predicate-name head prefix-ht))
    ((ruleml-forall :clause clause)
     (predicate-name clause prefix-ht))
    ((ruleml-exists :formula formula)
     (predicate-name formula prefix-ht))
    ((ruleml-const :contents (ruleml-pname-ln :name ns :url local))
     (make-url-const ns local prefix-ht))
    ((ruleml-const :contents const)
     const)))

(defun objectify (term relationships prefix-ht)
  (when (and (not psoatransrun::*static-objectification-only*)
             psoatransrun::*is-relational-p*)
    (return-from objectify term))
  (let* (vars
         (term (map-atom-transformer
                (lambda (term &rest args &key external &allow-other-keys)
                  (cond
                    (psoatransrun::*static-objectification-only*
                     (multiple-value-bind (term new-vars)
                         (apply #'objectify-static term args)
                       (appendf vars new-vars)
                       term))
                    ((and (ruleml-atom-p term) external)
                     term)
                    (t (if-it (predicate-name term prefix-ht)
                              (multiple-value-bind (tuple-arities foundp)
                                  (gethash it relationships)
                                (declare (ignore tuple-arities))
                                (if foundp
                                    (apply #'objectify-dynamic term relationships
                                           prefix-ht args)
                                    (multiple-value-bind (term new-vars)
                                        (apply #'objectify-static term args)
                                      (appendf vars new-vars)
                                      term)))
                              term))))
                term)))
    (match term
      ((ruleml-forall :vars orig-vars :clause clause)
       (make-ruleml-forall :vars (append vars orig-vars) :clause clause))
      (_ (if vars
             (make-ruleml-forall :vars vars :clause term)
             term)))))


(defun describute (term)
  (transform-ast term #'-describute))

(defun -describute (term &key &allow-other-keys)
  (match term
    ((ruleml-oidful-atom
      :oid oid
      :predicate (ruleml-atom :root root :descriptors descriptors))
     (let ((terms (nreverse
                   (cons
                    (make-ruleml-membership
                     :oid oid
                     :predicate root)
                    (mapcar #`(match %
                                ((or (ruleml-slot :dep t) (ruleml-tuple :dep t))
                                 (make-ruleml-oidful-atom
                                  :oid oid
                                  :predicate (make-ruleml-atom :root root
                                                               :descriptors (list %))))
                                ((or (ruleml-slot :dep nil) (ruleml-tuple :dep nil))
                                 (make-ruleml-oidful-atom
                                  :oid oid
                                  :predicate (make-ruleml-atom :root (make-ruleml-const :contents "Top")
                                                               :descriptors (list %)))))
                            descriptors)))))
       (if (single terms)
           (first terms)
           (make-ruleml-and :terms terms))))
    (_ term)))


(defun skolemize (term)
  (let (forall-vars)
    (labels ((vars-are-equal-p (item var)
               (match item
                 ((ruleml-var :name item-var-name)
                  (equal item-var-name (ruleml-var-name var)))))
             (skolem-term ()
               (if (null forall-vars)
                   (fresh-skolem-constant)
                   (make-ruleml-atom :root (fresh-skolem-constant)
                                     :descriptors (list (make-ruleml-tuple :dep t
                                                                           :terms forall-vars)))))
             (skolemize-head (term)
               (match term
                 ((ruleml-implies :conclusion conclusion :condition condition)
                  (make-ruleml-implies :conclusion (skolemize-head conclusion)
                                       :condition condition))
                 ((ruleml-exists :vars vars :formula formula)
                  (transform-ast formula
                                 (lambda (term &key &allow-other-keys)
                                   (if (member term vars :test #'vars-are-equal-p)
                                       (skolem-term)
                                       term))))
                 (_ term))))
      (match term
        ((ruleml-forall :vars vars :clause clause)
         (setf forall-vars (remove-if #'ruleml-genvar-p vars))
         (make-ruleml-forall :vars vars
                             :clause (skolemize-head clause)))
        (_ (skolemize-head term))))))


(defun flatten-externals (term)
  (let* ((vars)
         (term (map-atom-transformer
                (lambda (atomic-formula &rest args &key &allow-other-keys)
                  (multiple-value-bind (term new-vars)
                      (apply #'-flatten-externals atomic-formula args)
                    (appendf vars new-vars)
                    term))
                term)))
    (match term
      ((ruleml-forall :vars forall-vars :clause clause)
       (make-ruleml-forall :vars (append forall-vars vars) :clause clause))
      (_ (if (null vars)
             term
             (make-ruleml-forall :vars vars :clause term))))))

(defun -flatten-externals (atomic-formula &key &allow-other-keys)
  (let* (vars eqs
              (exprs (make-hash-table :test #'equalp)))
    (values (transform-ast
             atomic-formula
             (lambda (term &key external &allow-other-keys)
               (match term
                 ((ruleml-external :atom atom)
                  (let ((expr-var (multiple-value-bind (var foundp)
                                      (gethash atom exprs)
                                    (if foundp
                                        var
                                        (let ((var (fresh-variable)))
                                          (when external
                                            (push var vars)
                                            (setf (gethash atom exprs) var)))))))
                    (cond (external (push (make-ruleml-equal :left expr-var :right term) eqs)
                                    expr-var)
                          ((null eqs) term)
                          (t (prog1 (make-ruleml-and :terms (nreverse (cons term eqs)))
                               (setf eqs nil))))))
                 (_ term))))
            vars)))


(defun flatten-and (item)
  (match item
    ((ruleml-and :terms terms) (mapcan #'flatten-and terms))
    ((type list) (mapcan #'flatten-and item))
    (_ (list item))))

(defun split-conjuctive-conclusion (atomic-formula)
  (match atomic-formula
    ((ruleml-implies :conclusion (ruleml-and :terms terms)
                     :condition condition)
     (mapcar #`(make-ruleml-implies :conclusion %
                                    :condition condition)
             (flatten-and terms)))
    ((ruleml-forall :vars vars
                    :clause (ruleml-and :terms terms))
     (mapcar #`(make-ruleml-forall
                :vars vars
                :clause %)
             (flatten-and terms)))
    ((ruleml-forall :vars vars
                    :clause (ruleml-implies :conclusion (ruleml-and :terms terms)
                                            :condition condition))
     (mapcar #`(make-ruleml-forall
                :vars vars
                :clause (make-ruleml-implies :conclusion %
                                             :condition condition))
             (flatten-and terms)))
    (_ (flatten-and atomic-formula))))


(defun separate-existential-variables (term)
  (transform-ast term
                 (lambda (term &key positive negative &allow-other-keys)
                   (match term
                     ((ruleml-exists :vars vars :formula formula)
                      (if (and (not positive) negative) ;; We are in a condition.
                          (let ((renamed-evs (make-hash-table :test #'equalp)) ;; Hash table of renamed existential variables assigned to renamed-evs.
				)
                            (dolist (var vars)
                              (sethash (ruleml-var-name var) ;; Set the hash at the key (ruleml-var-name var) ...
                                       renamed-evs           ;; ... in the hash table renamed-evs ...
                                       (fresh-variable)))    ;; ... with the value generated by (fresh-variable).
                            (transform-ast formula
                                           (lambda (term &key &allow-other-keys)
                                             (if (ruleml-var-p term)
                                                 (multiple-value-bind (renamed-var foundp)
                                                     (gethash (ruleml-var-name term) renamed-evs)
                                                   (if foundp      ;; t iff the variable is named as a key in the hash table.
						       renamed-var ;; If the variable is found in the Exists formula, set it to its
						                   ;; new name.
						       term        ;; Not an existential variable, so use its original name.
						       ))
                                                 term))))
                          term))
                     (_ term)))))


(defun transform-document (document)
  (with ((prefix-ht (alist->ht (loop for prefix in (ruleml-document-prefixes document)
                                     collect (cons (ruleml-prefix-name prefix)
                                                   (ruleml-prefix-iri-ref prefix)))
                               :test #'equalp)))
    (make-ruleml-document
     :base (ruleml-document-base document)
     :prefixes (ruleml-document-prefixes document)
     :prefix-ht prefix-ht
     :imports (ruleml-document-imports document)
     :performatives
     (mapcar (lambda (term)
               (match term
                 ((ruleml-assert :items items)
                  (with ((relationships is-relational-p (kb-relationships term prefix-ht)))
                    (let ((*is-relational-p* is-relational-p))
                      (make-ruleml-assert
                       :items (mapcan #`(-> %
                                            subclass-rewrite
                                            embedded-objectify
                                            unnest
                                            (objectify relationships prefix-ht)
                                            skolemize
                                            separate-existential-variables
                                            describute
                                            flatten-externals
                                            split-conjuctive-conclusion)
                                      items)
                       :relationships relationships
                       :is-relational-p is-relational-p))))
                 ((ruleml-query)
                  (let ((relationships (make-hash-table :test #'equal)))
                    (transform-query term relationships prefix-ht)))
                 (_ term)))
             (ruleml-document-performatives document)))))


(defun transform-query (query relationships prefix-ht)
  (-> query
      embedded-objectify
      unnest
      (objectify relationships prefix-ht)
      separate-existential-variables
      describute))
