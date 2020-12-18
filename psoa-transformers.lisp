
(in-package #:psoa-transformers)

(named-readtables:in-readtable rutils-readtable)

#|
#:psoa-transformers defines a sequence of transformations between
RuleML ASTs. Different combinations of transformers-as-functions are
applied in anticipation of their output ASTs being translated to KBs
targeting a given logic engine. The precise combination of
transformations applied is determined by the logic engine
target.

For now, cl-psoatransrun exclusively targets Prolog engines. The
sequence of transformers applied for either of the Prolog engines it
currently supports (namely, XSB Prolog and Scryer Prolog) are defined
in the function transform-document for ruleml-document ASTs, and
transform-query for ruleml-query ASTs.
|#


#|
Global variables for command-line options.
|#

(defparameter *static-objectification-only* nil
  "If t, use static undifferentiated objectification during the
  objectify transformation.")

(defparameter *is-relational-p* nil
  "If t, the underlying PSOA RuleML KB, the subject of the
transformation, consists entirely of relational predicates. This
information can change the path of some transformations.")


#|
Functions defining transformations on PSOA RuleML KBs and queries, and their
utility functions.
|#

(defun fresh-variable (&optional (prefix "Var"))
  "Generate a fresh variable from an optional \"prefix\", using the gensym function,
so the result is guaranteed not to clash with previously existing
variable names. The string contents of the variable are wrapped in a
ruleml-genvar struct, a subtype of ruleml-var, to indicate that the
variable was machine generated and therefore not drawn from the source
KB/query."
  (make-ruleml-genvar :name (format nil "~A" (gensym prefix))))

(defun fresh-constant (&optional (prefix "_"))
  "Generate a new constant with an original name, not equal to any
previously existing, similarly to how fresh-variable works with
respect to variables."
  (make-ruleml-genconst :contents (string-downcase (format nil "~A" (gensym prefix)))))

(defun fresh-skolem-constant ()
  "Generate a fresh constant with prefix \"skolem\"."
  (fresh-constant "skolem"))

(defun make-ruleml-oidful (oid predicate &optional (position 0))
  "Construct an oidful RuleML atom whose type is determined by whether
the argument \"predicate\" is an oidless atom. As a rule, PSOA RuleML
atoms that written as \"o#p\" are always typed as
ruleml-membership's.

Since this rule is frequently enforced, it helps to have its logic
parceled into an appropriately parameterized function."
  (if (ruleml-atom-p predicate)
      (make-ruleml-oidful-atom :oid oid :predicate predicate :position position)
      (make-ruleml-membership :oid oid :predicate predicate :position position)))

(defun subclass-rewrite (term)
  "A transformer performed against top-level Assert items, rewriting
subclass relations ClassA##ClassB to

Forall ?o (
  ?o#ClassB :- ?o#ClassA
)

where ?o is a ruleml-genvar produced by the fresh-variable function."
  (match term
    ;; subclass directly in the Assert top-level
    ((ruleml-subclass-rel :super super :sub sub)
     (let* ((var   (fresh-variable))  ;; generate a fresh variable
	    ;; var as the oid, super as the predicate of rule head
	    (head  (make-ruleml-oidful var super))
	    ;; var as the oid, sub as the predicate of rule body
	    (body  (make-ruleml-oidful var sub)))
       ;; generate a Forall-wrapped rule
       ;; having only the generated variable inside the Forall
       (make-ruleml-forall :vars (list var)  ;; var as singleton variable declaration list
                           :clause (make-ruleml-implies :conclusion head
                                                        :condition  body))))
    ;; subclass wrapped in a Forall
    ((ruleml-forall :vars vars :clause (ruleml-subclass-rel :super super :sub sub))
     (let* ((var   (fresh-variable))
            (head  (make-ruleml-oidful var super))
            (body  (make-ruleml-oidful var sub)))
       (make-ruleml-forall :vars (cons var vars)  ;; var extends variable declaration list of vars
                           :clause (make-ruleml-implies :conclusion head
                                                        :condition  body))))
    (_ term)))


(defun map-atom-transformer (transformer term &rest args &key &allow-other-keys)
  "Walk the PSOA RuleML AST \"term\", applying the function argument
\"transformer\" only to nodes belonging to one of the types
ruleml-oidful-atom, ruleml-membership, ruleml-atom, or
ruleml-expr. Each of the nodes of these types is replaced by the
return value of the call to \"transformer\" taking the original node
as its first argument.

Many of the remaining PSOA transformers are defined in terms of
map-atom-transformer."
  (match term
    ((or (ruleml-oidful-atom) (ruleml-membership) (ruleml-atom) (ruleml-expr))
     (apply transformer term args))
    (_ (apply #'transform-ast term (lambda (term &key &allow-other-keys) term)
              :propagator (lambda (term &rest args &key &allow-other-keys)
                            (apply #'map-atom-transformer transformer term args))
              args))))


(defun embedded-objectify (term)
  "Generate for every oidless embedded atom in the AST \"term\" an OID
consisting of either a fresh variable or fresh constant, depending on
the KB/query context in which the atom appears. Oidless top-level
atoms are left unchanged by embedded-objectify, as their
objectification is the task of the later (top-level) objectify
transformation."
  (map-atom-transformer #'-embedded-objectify term))

(defun -embedded-objectify (atom &key positive negative &allow-other-keys)
  "-embedded-objectify targets one of the four types of PSOA RuleML
atom enumerated in the description of map-atom-transformer. It keeps
track of whether \"term\", the focus of the outermost pattern matching
form \"match\" in the \"walker\" local function, is embedded inside another
atom.

If it is, and it is an oidless atom, it is assigned:

1) a fresh constant OID if it occurs in a ground fact, or,
2) a fresh variable OID

In either case, \"term\" has its subterms similarly processed by
-embedded-objectify, before which in-psoa-rest-p is set to t, since its
subterms are synonymously embedded terms."
  (let (in-psoa-rest-p ;; initialize \"in-psoa-rest-p\" and \"embedded-oids\" to NIL.
        embedded-oids
        (ground-atom-p (ground-atom-p atom))) ;; check that atom is ground.
    (declare (special in-psoa-rest-p)) ;; in-psoa-rest is a dynamically scoped (special) variable.
    (labels ((substep (term &key &allow-other-keys) ;; term is embedded in an atom. Thus ...
               (let ((in-psoa-rest-p t)) ;; ... in-psoa-rest-p is t in all downward calls.
                 (declare (special in-psoa-rest-p))
                 (walker term)))
             (walker (term &key &allow-other-keys)
               (match term
                 ((ruleml-atom :root root :descriptors descriptors) ;; term is an oidless atom. .
                  (if in-psoa-rest-p ;; ... here embedded in another term. So ...
                      (let ((oid (if (and (not positive) (not negative) ground-atom-p) ;; ... generate an OID.
                                     (fresh-constant)
                                     (let ((var (fresh-variable)))
                                       ;; record the fresh variable OID for later use in an Exists wrapper.
                                       (push var embedded-oids)
                                       var))))
                        (make-ruleml-oidful-atom ;; attach an OID to the embedded oidless atom term.
                         :oid oid
                         :predicate
                         (make-ruleml-atom :root (walker root)
                                           :descriptors (mapcar #`(walker %) descriptors))))
                      (let ((in-psoa-rest-p t)) ;; otherwise, term is a top-level (non-embedded) atom.
                        ;; in-psoa-rest is now t, as we are about to recurse downward into term.
                        (declare (special in-psoa-rest-p))
                        ;; keep term as a top-level oidless atom, but
                        ;; objectify any atoms embedded within it.
                        (make-ruleml-atom :root (walker root)
                                          :descriptors (mapcar #`(walker %) descriptors)))))
                 ((ruleml-oidful-atom :oid oid :predicate predicate)
                  ;; term already has an OID, so recurse down into its subterms.
                  (make-ruleml-oidful
                   (substep oid)
                   (transform-ast predicate
                                  (lambda (term &key &allow-other-keys) term)
                                  :propagator #'substep)))
                 ((ruleml-membership :oid oid :predicate predicate)
                  ;; term already has an OID, so recurse down into its subterms.
                  (make-ruleml-oidful
                   (substep oid)
                   (transform-ast predicate
                                  (lambda (term &key &allow-other-keys) term)
                                  :propagator #'substep)))
                 (_
                  ;; keep term structurally intact, but objectify what
                  ;; are found to be its embedded atoms.
                  (transform-ast
                   term
                   (lambda (term &key &allow-other-keys) term)
                   :propagator #'walker)))))

      (let ((atom (walker atom)))
        (if (null embedded-oids)
            ;; initialize the transform by calling the \"walker\" local
            ;; function on the argument atom. If no fresh variables are generated,
            ;; just return the transformed atom.
            atom

            ;; otherwise, wrap the transformed atom in an Exists over the
            ;; list of variable OIDs generated in the course of the
            ;; \"walker\" call.
            (make-ruleml-exists :vars embedded-oids :formula atom))))))


(defun unnest (term)
  "The unnest transformer extracts all embedded oidful atoms out from
their embedded contexts onto the top-level, leaving in their place
only their OIDs. Naturally, unnest must be applied after the
embedded-objectify transformer."
  (map-atom-transformer #'-unnest term))

(defun -unnest (term &key &allow-other-keys)
  "The definition of -unnest is transcribed from section 5.2 of Gen
Zou's PhD thesis, \"Translators for Interoperating and Porting
Object-Relational Knowledge\". It consumes oidful atoms (that is,
atoms of subtype ruleml-membership or ruleml-oidful-atom) and returns
lists of their embedded atoms, whose OIDs are substituted for them in
the original atom, which becomes the last member of the list.

For example, the oidful atom

o1#p(o2#r(o3#q) o4#s)

is unnested as

And(
 o3#q o2#r(+[o3]) o4#s o1#p(+[o2 o4])
)"
  (let (trimmed-terms)
    (labels ((check-oid (oid)
               ;; fresh variables replace anonymous variables, and fresh constants
               ;; replace "anonymous" constants (i.e., those named "_").
               (match oid
                 ((ruleml-var :name name)
                  (if (string= name "")
                      (fresh-variable)
                      oid))
                 ("_" (fresh-constant))
                 (_ oid)))
             (atoms (term &key &allow-other-keys)
               (match term
                 ;; only oidful atoms are submitted for trimming.
                 ((or (ruleml-oidful-atom) (ruleml-membership))
                  (push (trim term) trimmed-terms)))
               term)
             (trim (term)
               ;; trim and retain are mutually recursive functions that search
               ;; for oidful atoms to be unnested and substituted for in their
               ;; original context by their OIDs.
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
                  ;; retain the OIDs of oidful atoms occurring as
                  ;; subterms, but add the similarly trimmed remains
                  ;; to the list trimmed-terms.
                  (retain oid))
                 ((or (ruleml-atom) (ruleml-expr) (ruleml-slot) (ruleml-tuple))
                  ;; otherwise, unnest the subterms of \"term\".
                  (trim term))
                 (_ term))))

      ;; initiate the unnesting.
      (match term
        ((or (ruleml-oidful-atom) (ruleml-atom) (ruleml-membership) (ruleml-expr))
         (transform-ast term #'atoms)
         (when (ruleml-atom-p term)
           (push (trim term) trimmed-terms))))

      ;; return the unnested terms as the conjuncts of an And(...) formula
      ;; if more than one term is in trimmed-terms.
      (cond ((null trimmed-terms) term)
            ((single trimmed-terms) (first trimmed-terms))
            (t (make-ruleml-and :terms (nreverse trimmed-terms)))))))


(defun objectify-static (term &key positive negative external &allow-other-keys)
  "Perform the first kind of top-level objectification, static
differentiated objectification, \"static\" because it occurs at
compile-time, and \"differentiated\" because the kind of OID generated
varies by KB/query context.

Specifically:

1) if the top-level atom being objectified is a ground fact,
it is given a fresh constant OID.

2) if the top-level atom being objectified occurs in a query or rule
conclusion, it is given the fresh variable OID ?o, which is an
existential variable in the target clause Exists ?o (...), where the
... is original atom, now tagged with the OID ?o.

3) otherwise, generate a fresh variable OID. Since it must occur in a
rule conclusion, the variable OID is added to the variables of an
enclosing Forall clause, which is created afresh if it didn't exist
before objectify-static was applied to it.

objectify-static does not have to distinguish between top-level and
embedded atoms, since all oidless embedded atoms were objectified by
the preceding embedded-objectify transformation."
  (let (vars)
    (values
     (match term
       ((guard (or (ruleml-atom) (ruleml-const))
               (not external))
        (let ((ground-atom-p (ground-atom-p term)))
          (cond ((and ground-atom-p (not positive) (not negative)) ; 1
                 (make-ruleml-oidful (fresh-constant)
                                     (if (ruleml-const-p term)
                                         (make-ruleml-atom :root term)
                                         term)))
                ((or (not (or ground-atom-p positive negative))
                     (and positive negative)
                     positive)          ; 2
                 (let ((var (fresh-variable)))
                   (make-ruleml-exists
                    :vars (list var)
                    :formula (make-ruleml-oidful var term))))
                (t (let ((var (fresh-variable))) ; 3
                     (push var vars)
                     (make-ruleml-oidful var term))))))
       (_ term))
     vars)))


(defun make-oid-cons (root terms)
  "objectify-dynamic sometimes uses this function to Skolem-create
function term OIDs for atoms left oidless in the KB."
  (make-ruleml-atom :root (make-ruleml-const :contents "_oid_cons")
                    :descriptors (list (make-ruleml-tuple :dep t :terms (cons root terms)))))

(defun is-relationship-p (term prefix-ht)
  "Similarly to ground-atom-p, search the internals of \"term\" for
proof that it is not a relationship. is-relationship-p is always
called in the context of a KB atom. If term contains generated
variables or generated constants, term is presumed to contain atom
OIDs, making it non-relational."
  ;; if-it is an anaphoric macro binding the return value of
  ;; (predicate-name term prefix-ht) to the symbol "it" in the scope
  ;; of its true branch form.
  (if-it (predicate-name term prefix-ht)
         (transform-ast term
                        (lambda (term &key &allow-other-keys)
                          (match term
                            ((ruleml-atom :descriptors descriptors)
                             (when (and (string= it (predicate-name term prefix-ht))
                                        ;; by definition,
                                        ;; relationships have a
                                        ;; dependent tuple as their
                                        ;; only descriptor.
                                        (not (and (single descriptors)
                                                  (ruleml-tuple-p (first descriptors))
                                                  (ruleml-tuple-dep (first descriptors)))))
                               (return-from is-relationship-p nil))
                             term)
                            ((or (ruleml-oidful-atom) (ruleml-membership))
                             (when (string= it (predicate-name term prefix-ht))
                               (return-from is-relationship-p nil))
                             term)
                            ((or (ruleml-genvar) (ruleml-genconst))
                             (return-from is-relationship-p nil))
                            (_ term))))
         (return-from is-relationship-p nil))
  t)

(defun objectify-dynamic (term relationships prefix-ht &key positive negative &allow-other-keys)
  "In the default static/dynamic objectification transformation,
top-level KB and query atoms are sometimes deferred from
objectification at compile-time, leaving the objectification work to
objectify-dynamic, which is used to introduce ad hoc OIDs at query
time. In this way, we avoid needlessly objectifying relational
predicates."
  (if (and positive negative) ;; we are objectifying a query atom.
      (match term
        ((ruleml-atom :root root :descriptors descriptors)
         (cond ((when-it (gethash root relationships)
                  (and (is-relationship-p term prefix-ht)
                       (member (length (ruleml-tuple-terms (first descriptors)))
                               it)))
                term) ;; the atom is a relational predicate call. leave it unobjectified/oidless.
               ((some (lambda (term)
                        (or (ruleml-slot-p term)
                            (not (ruleml-tuple-dep term))))
                      descriptors)
                (make-ruleml-or)) ;; the atom is not a relational predicate call. replace it with Or().
               (t
                (let ((oid (fresh-variable))) ;; the atom is relational, but doesn't pertain to
                  (make-ruleml-exists ;; a relational predicate call. objectify it and
                   :vars (list oid) ;; wrap it in an Exists ?o (...). invoke objectify-dynamic
                   :formula (objectify-dynamic ;; on the oidful atom just obtained.
                             (make-ruleml-oidful-atom :oid oid :predicate term)
                             relationships
                             prefix-ht
                             :positive positive
                             :negative negative))))))
        ((ruleml-oidful-atom ;; term already has an OID.
          :oid oid
          :predicate (ruleml-atom :root root :descriptors descriptors))
         (cond ((or (not (ruleml-var-p oid)) ;; term either doesn't have a variable OID
                    (some (lambda (term)     ;; or is non-relational.
                            (or (ruleml-slot-p term)
                                (not (ruleml-tuple-dep term))))
                          descriptors))
                (make-ruleml-or)) ;; replace it with Or().
               (t ;; unify the variable OID of term to a function term
                ;; OID Skolem-created by make-oid-cons.
                (if descriptors
                    (make-ruleml-and
                     :terms
                     (loop for tuple in descriptors
                           for terms = (ruleml-tuple-terms tuple)
                           for atom  = (make-ruleml-atom :root root :descriptors (list tuple))
                           for oid-cons-equal = (make-ruleml-equal
                                                 :left  oid
                                                 :right (make-oid-cons root terms))
                           nconc (list atom oid-cons-equal)))
                    (make-ruleml-equal
                     :left oid
                     :right (make-oid-cons root '()))))))
        ((ruleml-membership :oid oid :predicate predicate) ;; term is a membership.
         (if (ruleml-var-p oid)
             (multiple-value-bind (tuple-arities foundp) ;; 2.4
                 (gethash (predicate-name predicate prefix-ht) relationships)
               (if foundp
                   ;; if term is relational, then construct an Or(...)
                   ;; whose disjuncts are oidful atoms containing a
                   ;; single dependent tuple. each dependent tuple
                   ;; contains k fresh variables, where k is drawn
                   ;; from the list of unique tuple arities spanned by
                   ;; the definition of predicate in the KB.
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
             ;; if term is non-relational, replace it with an Or().
             (make-ruleml-or)))
        (_ term))

      ;; term is not in a query, so leave it unchanged.
      term))

(defun kb-relationships (ruleml-assert prefix-ht)
  "A utility function to scan a RuleML Assert AST node, and, with the
help of its prefix hash table \"prefix-ht\", construct the
relationships hash table describing it relational predicates.

The relationships table takes as its key the predicate name and maps
to the list of unique dependent tuple arities spanned by that
predicate in the KB. A tuple arity is the length of any one of the
predicate's dependent tuple descriptors.

The list of Assert items is scanned and any apparently relational
predicates it has are detected by the is-relational-p predicate
function. If an apparently relational predicate is later found to have
a non-relational variant, its name is added to the so-called
blacklist, which is ultimately used to remove it from the
relationships hash table just before kb-relationships
returns. Predicates described in the relationships table which never
appear in the blacklist are thereby proven relational.

Therefore, the KB is relational iff blacklist is NIL when kb-relationships
returns. The second return value of kb-relationships is used to set
the value of the *is-relational-p* special variable."
  (let ((relationships (make-hash-table :test #'equal))
        (blacklist) ;; blacklist is initialized to NIL/().
        )
    (labels ((consider-atom (term head-atom)
               (if (is-relationship-p term prefix-ht)
                   ;; pushnew refrains from pushing a value to a list
                   ;; if it finds it's already a member of the list.
                   (pushnew (length (ruleml-tuple-terms (first (ruleml-atom-descriptors head-atom))))
                            (gethash (predicate-name head-atom prefix-ht) relationships))
                   ;; prefix-ht is used to calculate predicate names for relationship keys.
                   (push (predicate-name head-atom prefix-ht) blacklist)))
             (consider-assert-item (item &key &allow-other-keys)
               (match item
                 ((or (ruleml-oidful-atom) (ruleml-atom))
                  (consider-atom item item))
                 ((ruleml-implies :conclusion (ruleml-atom))
                  (consider-atom item (ruleml-implies-conclusion item)))
                 ((ruleml-forall :clause clause)
                  (consider-assert-item clause)))
               item))

      ;; use transform-ast to find every instance where a predicate may be
      ;; used in a non-relational manner, and when found, blacklist it.
      (transform-ast ruleml-assert #'consider-assert-item)

      ;; remove the blacklisted predicate names from relationships.
      (dolist (root blacklist)
        (remhash root relationships))

      ;; return values: relationship hash table, boolean that is t iff
      ;; the Assert is completely relational.
      (values relationships (null blacklist)))))

(defun predicate-name (atom prefix-ht)
  "Compute the proper name of a predicate, the root predicate name of
a fact or rule conclusion, whether it is an oidless or oidful atom, a
membership, inside an Exists or Forall clause, etc."
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
     (write-url-const ns local prefix-ht))
    ((ruleml-const :contents const)
     const)))

(defun objectify (term relationships prefix-ht)
  "By default, the objectify transformation applies the following mixed mode
static/dynamic objectification to \"term\":

objectify-dynamic is applied if \"term\" pertains to relational KB
predicate identified using the \"relationships\" argument, a hash
table. Otherwise, objectify-static is applied to \"term\".

Static/dynamic objectificatoin is switched off by toggling the
*static-objectification-only* special variable to NIL, in which case
objectify-static is always used.

If in addition to dynamic objectification being deferred the KB is
relational (true iff *is-relational-p* is t), the objectify
transformation is not applied at all."
  (when (and (not *static-objectification-only*)
             *is-relational-p*)
    (return-from objectify term))
  (let* (vars
         (term (map-atom-transformer
                (lambda (term &rest args &key external &allow-other-keys)
                  (cond
                    (*static-objectification-only*
                     (multiple-value-bind (term new-vars)
                         (apply #'objectify-static term args)
                       (appendf vars new-vars) ;; append new-vars to the Forall clause vars.
                       term))
                    ((and (ruleml-atom-p term) external) ;; don't objectify atoms in External(...)'s.
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
  "describute distributes the descriptors of an oidful atom across
distinct oidful atoms which share the OID and atom root of the
descributed atom, including that without any descriptors (i.e., a
membership). The resulting atoms are gathered as the conjuncts of a
single And(...) formula if there are more than one.

For example, the atom

o#p(+[a b c] -[d e f] s1->v1 s2+>v2)

is descributed as

And(o#p(+[a b c]) o#Top(-[d e f]) o#Top(s1->v1) o#p(s2+>v2) o#p)

Note that \"Top\" is substituted for \"p\" when the descriptor is
independent."
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
                                  :predicate (make-ruleml-atom
                                              :root (make-ruleml-const :contents "Top")
                                              :descriptors (list %)))))
                            descriptors)))))
       (if (single terms)
           (first terms)
           (make-ruleml-and :terms terms))))
    (_ term)))


(defun skolemize (term)
  "skolemize replaces the existential variables specified by
Exists(...) formulas in rule conclusions and facts with function terms
rooted with a fresh constant prefixed by \"skolem\". The function
terms' arguments are the non-generated variables of the enclosing
Forall clause, if there is one."
  (let (forall-vars)
    (labels ((skolem-term ()
               (if (null forall-vars)
                   (fresh-skolem-constant)
                   (make-ruleml-expr :root (fresh-skolem-constant)
                                     :terms (list (make-ruleml-tuple :dep t
                                                                     :terms forall-vars)))))
             (skolemize-head (term &key positive negative &allow-other-keys)
               (match term
                 ((ruleml-implies :conclusion conclusion :condition condition :position pos)
                  (make-ruleml-implies :conclusion (skolemize-head conclusion :positive t)
                                       :condition condition
                                       :position pos))
                 ((ruleml-and :terms terms)
                  (make-ruleml-and
                   :terms (mapcar #`(skolemize-head % :positive positive :negative negative)
                                  terms)))
                 ((guard (ruleml-exists :vars vars :formula formula)
                         (or positive (not negative))) ;; the exists is in a conclusion or fact.
                  (let ((vars (alist->ht (mapcar #`(cons (ruleml-var-name %) (skolem-term))
                                                 vars)
                                         :test #'equalp)))
                    ;; replace the existential variables in the
                    ;; enclosed existential formula with the generated
                    ;; skolem terms, indexed by the corresponding
                    ;; variable in the vars hash-table. once that's
                    ;; done, skolemize the variables of any remaining
                    ;; existentials from the outer call to
                    ;; transform-ast.
                    (transform-ast
                     (transform-ast formula
                                    (lambda (term &key &allow-other-keys)
                                      (if (ruleml-var-p term)
                                          (multiple-value-bind (skolem-term foundp)
                                              (gethash (ruleml-var-name term) vars)
                                            (if foundp
                                                skolem-term
                                                term))
                                          term)))
                     #'skolemize-head
                     :positive positive
                     :negative negative)))
                 (_ term))))
      (match term
        ((ruleml-forall :vars vars :clause clause)
         (setf forall-vars (remove-if #'ruleml-genvar-p vars))
         (make-ruleml-forall :vars vars :clause (skolemize-head clause :positive t)))
        (_ (skolemize-head term :positive t))))))


(defun -flatten-externals (term)
  (let ((eqs)
        (gen-vars))
    (labels ((trim (term &key external &allow-other-keys)
               (match term
                 ((guard (or (ruleml-oidful-atom) (ruleml-atom)
                             (ruleml-expr) (ruleml-subclass-rel)
                             (ruleml-equal) (ruleml-external))
                         (not external))
                  (let ((term (transform-ast term #'retain)))
                    (prog1 (if eqs
                               (make-ruleml-and :terms (nreverse (cons term eqs)))
                               term)
                      (setf eqs nil))))
                 (_ (transform-ast term (lambda (term &key &allow-other-keys) term)
                                   :propagator #'trim
                                   :external external))))
             (retain (term &key external &allow-other-keys)
               (match term
                 ((guard (ruleml-external :atom (ruleml-expr))
                         external)
                  ;; term is an expression inside an External.
                  (let ((expr-var (fresh-variable)))
                    (push (make-ruleml-equal ;; push an equality between expr-var and
                           :left expr-var ;; the expression to the eqs list.
                           :right term)
                          eqs)
                    (push expr-var gen-vars)
                    expr-var))
                 (_ term))))
      (values (trim term) gen-vars))))

(defun flatten-externals (term)
  "External(...) formulas contain expressions meant to be evaluated
using the call-by-value evaluation strategy.

flatten-externals extracts and orders the subexpressions of the
External formula by their evaluation order and binds
intermediary results to fresh variables for use in later expressions.

If an External is flattened within a query, the variables generated in
flatten-externals are quantified as the existential variables of an
Exists formula containing the External. Otherwise, the generated
variables are added to a Forall clause containing the transformed rule
or fact of the KB.

The action of flatten-externals and its helper -flatten-externals are
defined according to section 5.7 of Gen Zou's thesis."
  (let* ((vars)
         (queryp (ruleml-query-p term)))
    (labels ((flattener (term)
               (match term
                 ((ruleml-implies :conclusion conclusion :condition condition)
                  (make-ruleml-implies :conclusion conclusion
                                       :condition (flattener condition)))
                 (_ (multiple-value-bind (flattened-term new-vars)
                        (-flatten-externals term)
                      (let ((term (typecase flattened-term
                                    (list (make-ruleml-and :terms flattened-term))
                                    (t flattened-term))))
                        (cond ((and queryp new-vars)
                               (make-ruleml-exists :vars new-vars :formula term))
                              (new-vars (appendf vars new-vars)
                                        term)
                              (t term))))))))
      (match term
        ((ruleml-forall :vars forall-vars :clause clause)
         (let ((clause (flattener clause)))
           (make-ruleml-forall :vars (append forall-vars vars)
                               :clause clause)))
        ((ruleml-query :term term)
         (make-ruleml-query :term (flattener term)))
        (_ (let ((term (flattener term)))
             (if vars
                 (make-ruleml-forall :vars vars :clause term)
                 term)))))))

(defun flatten-and (item)
  "A utility function that flattens And(...) terms within lists."
  (match item
    ((ruleml-and :terms terms) (mapcan #'flatten-and terms))
    ((type list) (mapcan #'flatten-and item))
    (_ (list item))))

(defun split-conjuctive-conclusion (atomic-formula)
  "After all the preceding transformations, it's possible for a PSOA
RuleML KB to have rules with And formulas for conclusions. Since some
logic engine targets (Prolog among them) don't support conjunctive
conclusions, conjunctive conclusions on the PSOA RuleML side must be
somehow dealt with.

The solution offered by split-conjuctive-conclusion is to create a
copy of the rule whose condition matches that of the original but
whose conclusion is one of the conjuncts of the original And formula
conclusion.

Since multiple atoms can be so obtained from a single atom,
split-conjuctive-conclusion returns a list of its results.

Redundantly embedded And formulas are flattened using flatten-and to
ensure conjuctive conclusions are truly split."
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
  "Exists formulas offer a kind of lexical scoping for logical
variables. Often no such concept exists in the target logic engine --
for instance, in Prolog, all variables are dynamically
scoped.

separate-existential-variables removes Exists formulas on the
condition side only (recall that Exists formulas on the conclusion
side are eliminated by skolemization). It generates a fresh variable
for every existentially qualified variable, and substitutes it for
every occurrence of the existential variable in the Exists
subformula.

The post-order traversal of transform-ast implements the shadowing of
variable names entailed by lexical scoping."
  (transform-ast
   term
   (lambda (term &key negative &allow-other-keys)
     (match term
       ((ruleml-exists :vars vars :formula formula)
        (if negative ;; we are in a condition or query.
            (let ((renamed-evs (make-hash-table :test #'equalp)) ;; hash table of renamed existential variables assigned to renamed-evs.
				  )
              (dolist (var vars)
                (sethash (ruleml-var-name var) ;; set the hash at the key (ruleml-var-name var) ...
                         renamed-evs ;; ... in the hash table renamed-evs ...
                         (fresh-variable))) ;; ... with the value generated by (fresh-variable).
              (transform-ast
               formula
               (lambda (term &key &allow-other-keys)
                 (if (ruleml-var-p term)
                     (multiple-value-bind (renamed-var foundp)
                         (gethash (ruleml-var-name term) renamed-evs)
                       (if foundp ;; t iff the variable is named as a key in the hash table.
						   renamed-var ;; if the variable is found in the Exists formula, set it to its
						   ;; new name.
						   term ;; not an existential variable, so use its original name.
						   ))
                     term))))
            term))
       (_ term)))))


(defun constant-name-generator (excluded-constants)
  "Generate the next constant in the sequence _1, _2, _3,
   ... that hasn't been generated already and that doesn't
   appear in \"excluded-constants.\""
  (let ((counter 1))
    (lambda ()
      (loop (let ((string (format nil "_~D" counter)))
              (incf counter)
              (unless (or (gethash string excluded-constants)
                          (find-symbol string))
                (return (make-ruleml-const :contents string))))))))


(defun rename-anonymous-constants (term name-generator)
  "Rename anonymous constants using the function \"name-generator\"."
  (transform-ast term
                 (lambda (term &key &allow-other-keys)
                   (match term
                     ((ruleml-const :contents "_")
                      (funcall name-generator))
                     (_
                      term)))))

(defun rename-local-constants (term excluded-constants)
  "Rename local constants in a RuleML Assert item according to the
convention _1, _2, ..., _N, if the names aren't already taken."
  (let ((new-constant (constant-name-generator excluded-constants))
        (new-constant-names (make-hash-table :test #'equal)))
    (transform-ast
     term
     (lambda (term &key &allow-other-keys)
       (match term
         ;; local constants are always ruleml-const values with
         ;; string \"contents\".
         ((guard (ruleml-const :contents contents)
                 (stringp contents))
          (multiple-value-bind (new-constant foundp)
              (gethash contents new-constant-names)
            (if foundp
                ;; use the previously generated name if one is found in new-constant-names.
                new-constant
                ;; generate a new name and index it in new-constant-names.
                (sethash contents new-constant-names (funcall new-constant)))))
         (_
          term))))))

(defun fetch-psoa-url (url)
  "Fetch a PSOA KB at the URL and return it as a string."
  ;; the URL must end in ".psoa".
  (assert (ends-with ".psoa" url))
  ;; drakma:http-request will return a vector of character codes, so we
  ;; convert it into a string.
  (map 'string #'code-char (drakma:http-request url)))

(defun index-local-constants (term)
  "Index the local constants of \"master-document\" in the
  \"excluded-constants\" hash table."
  (let ((excluded-constants (make-hash-table :test #'equal)))
    (transform-ast
     term
     (lambda (term &key &allow-other-keys)
       (match term
         ;; The conditions of this guard pattern are necessary and
         ;; sufficient to qualify a constant as a local constant.
         ((guard (ruleml-const :contents contents)
                 (stringp contents))
          (sethash contents excluded-constants t)))
       term))
    excluded-constants))

(defun merge-and-rename (master-document imported-documents)
  "Merge the documents in the list of ruleml-documents
\"imported-documents\" into the ruleml-document
\"master-document\". Rename the local constants of the imported
documents to avoid clashes with those of \"master-document\". Return
the resulting merged document."
  (let ((excluded-constants (index-local-constants master-document))
        ;; master-document may not have an Assert performative; if
        ;; not, create one for it.
        (master-document-assert
          (first
           (if-it (ruleml-document-performatives master-document)
                  it
                  (push (make-ruleml-assert)
                        (ruleml-document-performatives master-document))))))

    (check-type master-document-assert ruleml-assert)

    (dolist (imported-document imported-documents)
      ;; rename the local constants of \"imported-document\", and merge
      ;; it Assert items and prefixes into \"master-document\".
      (let ((imported-document (rename-local-constants imported-document excluded-constants)))
        (appendf (ruleml-document-prefixes master-document)
                 (ruleml-document-prefixes imported-document))
        (appendf (ruleml-assert-items master-document-assert)
                 (ruleml-assert-items (first (ruleml-document-performatives
                                              imported-document))))))

    master-document))

(defun load-imports-and-merge (document)
  "Load the Imports of the ruleml-document \"document\" and rename all
local constants before merging them all into a single ruleml-document,
which is returned."
  ;; copy the list of imports from \"document\" so that we can
  ;; mutate the local copy as a queue. imports is a shallow copy of the
  ;; imports list.
  (let ((imports (copy-list (ruleml-document-imports document)))
        (documents '()))

    ;; the do macro is documented in the Common Lisp Hyperspec here:
    ;; http://www.lispworks.com/documentation/HyperSpec/Body/m_do_do.htm
    (do ((import (pop imports) (pop imports)))
        ((null import) (merge-and-rename document (nreverse documents)))
      (let* ((url (ruleml-import-iri-ref import))
             (document (fetch-psoa-url (ruleml-iri-contents url)))
             (document (parse 'psoa-grammar::ruleml document)))
        (appendf imports (ruleml-document-imports document))
        (push document documents)))))

(defun transform-document (document)
  "Consumes a document (an instance of ruleml-document) and transforms
it according to the transformation/normalization steps described in
this package, defined in PSOATransRun papers and in Gen Zou's PhD
thesis.

The return value is another ruleml-document instance containing the
transformed KB in addition to newly created relationships and
prefix-ht hash tables."
  (let* ((document (load-imports-and-merge document))
         (local-constants (index-local-constants document))
         (anonymous-constant-renamer (constant-name-generator local-constants))
         (prefix-ht (prefix-list->prefix-ht (ruleml-document-prefixes document))))
    (make-ruleml-document
     :base (ruleml-document-base document)
     :prefixes (ruleml-document-prefixes document)
     :prefix-ht prefix-ht
     :imports (ruleml-document-imports document)
     :performatives
     (mapcar (lambda (term)
               (match term
                 ((ruleml-assert :items items)
                  ;; -> is a Common Lisp implementation of the -> threading macro of
                  ;; Clojure. Here it is a more readable, decompactified way of writing
                  ;; (unnest (embedded-objectify (subclass-rewrite
                  ;;                               (rename-anonymous-constants %
                  ;;                                 anonymous-constant-renamer))).
                  (let ((first-stage-items (mapcar #`(-> (rename-anonymous-constants
                                                          %
                                                          anonymous-constant-renamer)
                                                         subclass-rewrite
                                                         embedded-objectify
                                                         unnest)
                                                   items)))
                    ;; new relationships were potentially created by
                    ;; the first three transforms, so only now can we
                    ;; calculate the relationships hash table of the
                    ;; KB.
                    (multiple-value-bind (relationships is-relational-p)
                        (kb-relationships (make-ruleml-assert :items first-stage-items)
                                          prefix-ht)
                      (let ((*is-relational-p* is-relational-p))
                        ;; binding the *is-relational-p* special variable for use by
                        ;; objectify and subsequent transformations.
                        (make-ruleml-assert
                         :items (mapcan #`(-> (objectify % relationships prefix-ht)
                                              skolemize
                                              flatten-externals
                                              separate-existential-variables
                                              describute
                                              split-conjuctive-conclusion)
                                        first-stage-items)
                         :relationships relationships
                         :is-relational-p is-relational-p)))))
                 ((ruleml-query)
                  (let ((relationships (make-hash-table :test #'equal)))
                    (transform-query term relationships prefix-ht)))
                 (_ term)))
             (ruleml-document-performatives document)))))



(defun transform-query (query relationships prefix-ht)
  "Consume an instance of ruleml-query and return a transformed
instance of ruleml-query, according to the sequence of operations
below, using the Clojure-style (->) threading macro discussed in
the documentation of transform-document."
  ;; the transformations which apply to queries are slightly fewer
  ;; than those that apply to KB items.
  (-> query
      embedded-objectify
      unnest
      (objectify relationships prefix-ht)
      flatten-externals
      separate-existential-variables
      describute))
