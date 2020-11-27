
(in-package #:psoa-grammar)

#|
This package specifies EBNF-like grammar rules for parsing PSOA
RuleML, documents and queries into abstract syntax trees (AST)
preserving their structure and content. The types of the PSOA RuleML
AST nodes constructed by the rules are defined in the package
#:psoa-ast.

The rules themselves are specified in the rule notation of the esrap
library, a backtracking packrat parser generator for Common Lisp,
documented at:

https://scymtym.github.io/esrap/

The rules are in nearly one-to-one correspondence with the PSOA RuleML
grammar specified at this link in EBNF notation:

http://wiki.ruleml.org/index.php/PSOA_RuleML#Monolithic_EBNF_for_PSOA_RuleML_Presentation_Syntax
|#

;; The rule language.

(let (*prefix-ht*)
  (declare (special *prefix-ht*))
  #|
  *prefix-ht* is designated as a special, or dynamic, variable. This
  causes its binding to behave like that of a global variable, except
  that its value is determined by its most recent binding in a let
  form further up the call stack. Once the thread of control exits
  the scope of that let, the binding is automatically undone and the
  value of the variable is determined by its next most recent
  binding up the call stack, if there is one. In this way, dynamic scoping
  emulates the stack-like variable shadowing of lexical scope, but the
  range of its shadowing isn't lexically confined.

  Therefore, the value of *prefix-ht* will reset automatically once
  the ruleml production concludes. It is bound to a prefix hash table
  in the prefix-list production, which is then accessible -- almost
  exactly as a global variable would be -- to later productions in the
  parse of the RuleML document.
  |#
  (defrule ruleml
      (and (* (or whitespace comment))
           (or "RuleML" "Document")
           (* (or whitespace comment))
           #\(
           (* (or whitespace comment))
           (? base)
           prefix-list
           (* (or import whitespace comment))
           (* (or assert query whitespace comment))
           #\)
           (* (or whitespace comment)))
    (:destructure (ws1 ruleml ws2 lparen ws3 base prefixes imports performatives rparen ws4
                       &bounds start)
      (declare (ignore ws1 lparen ws2 rparen ws3 ws4))

      ;; Issue deprecation warning for use of Document in place of RuleML.
      (when (string= ruleml "Document")
        (warn "\"Document\" is deprecated, use \"RuleML\" instead."))

      (make-ruleml-document :base base
                            :prefixes prefixes ;; NIL's were removed from within prefix-list
                            :prefix-ht *prefix-ht*
                            :imports (remove nil imports)
                            :performatives (remove nil performatives)
                            :position start))))

(defrule prefix-list
    (* (or prefix whitespace comment))
  (:around ()
           (declare (special *prefix-ht*))
           (let ((prefix-list (esrap:call-transform)))
             (setf *prefix-ht* (prefix-list->prefix-ht prefix-list))
             prefix-list))
  (:lambda (prefixes)
    (remove nil prefixes)))

(defrule multi-line-comment
    (or (and "<!--" (* (and (! "-->") character)) "-->")
        (and "/*" (* (and (! "*/") character)) "*/"))
  (:constant nil))

(defrule single-line-comment
    (and #\% (* (and (! #\Newline) character)))
  (:constant nil))

(defrule comment
    (or single-line-comment
        multi-line-comment))

(defrule base
    (and "Base"
         (* (or whitespace comment))
         #\(
         (* (or whitespace comment))
         iri-ref
         (* (or whitespace comment))
         #\)
         (* (or whitespace comment)))
  (:destructure (base ws1 lparen ws2 iri-ref ws3 rparen ws4 &bounds start)
    (declare (ignore base ws1 lparen ws2 ws3 rparen ws4))
    (make-ruleml-base :iri-ref iri-ref :position start)))

(defrule prefix
    (and "Prefix"
         (* (or whitespace comment))
         #\(
         (* (or whitespace comment))
         (? name)
         (* (or whitespace comment))
         #\:
         (* (or whitespace comment))
         iri-ref
         (* (or whitespace comment))
         #\))
  (:destructure (prefix ws1 lparen ws2 name ws3 colon ws4 iri-ref ws5 rparen
                        &bounds start)
    (declare (ignore prefix ws1 lparen ws2 ws3 colon ws4 rparen ws5))
    (make-ruleml-prefix :name name :iri-ref iri-ref :position start)))

(defrule import
    (and "Import"
         (* (or whitespace comment))
         #\(
         (* (or whitespace comment))
         iri-ref
         (* (or whitespace comment))
         (? profile)
         (* (or whitespace comment))
         #\)
         (* (or whitespace comment)))
  (:destructure (import ws1 lparen ws2 iri-ref ws3 profile ws4 rparen ws5 &bounds start)
    (declare (ignore import ws1 lparen ws2 ws3 rparen ws4 ws5))
    (make-ruleml-import :iri-ref iri-ref
                        :profile profile
                        :position start)))

(defrule assert
    (and (or "Assert" "Group")
         (* (or whitespace comment))
         #\(
         (* (or rule expr-long whitespace comment))
         #\)
         (* (or whitespace comment)))
  (:destructure (assert ws1 lparen rules rparen ws2 &bounds start)
    (declare (ignore ws1 lparen rparen ws2))

    ;; Issue deprecation warning for use of Group in place of Assert.
    (when (string= assert "Group")
      (warn "\"Group\" is deprecated, use \"Assert\" instead."))

    (make-ruleml-assert :items (remove nil rules) :position start)))

(defrule query
    (and "Query"
         (* (or whitespace comment))
         #\(
         (* (or whitespace comment))
         formula
         (* (or whitespace comment))
         #\)
         (* (or whitespace comment)))
  (:destructure (query ws1 lparen ws2 formula ws3 rparen ws4 &bounds start)
                (declare (ignore query ws1 lparen ws2 ws3 rparen ws4))
                (make-ruleml-query :term formula :position start)))

(defun check-variable-quantification (original-formula)
  "Issue warnings for Assert formulas in two cases: if a non-ground
atom isn't enclosed in a Forall formula, and if the enclosing Forall
of an atom doesn't quantify a variable inside the atom."
  (let ((*quantified-vars*)
        (unquantified-vars))
    (declare (special *quantified-vars*))
    (labels ((checker (formula &key &allow-other-keys)
               ;; checker maintains a list, \"*quantified-vars*\", of
               ;; quantified variable names while performing a pre-order
               ;; traversal of the AST node \"formula\".  Upon
               ;; encountering a variable, checker detects if it is
               ;; missing from \"*quantified-vars*\". The names of
               ;; variables missing from \"*quantified-vars*\" are pushed
               ;; afresh to \"unquantified-vars\".
               (match formula
                 ((ruleml-exists :vars exists-vars :formula formula)
                  (let* ((exists-vars (mapcar #'ruleml-var-name exists-vars))
                         (*quantified-vars* (append exists-vars *quantified-vars*)))
                    ;; \"exist-vars\" is prepended onto \"*quantified-vars*\"
                    ;; until the recursive call of checker returns, at
                    ;; which point \"*quantified-vars*\", as a dynamic
                    ;; variable, reverts to its next most recent binding
                    ;; up the call stack
                    (declare (special *quantified-vars*))
                    (checker formula)))
                 ((guard (ruleml-var :name name)
                         (not (ruleml-genvar-p formula)))
                  ;; unless \"name\" belongs to \"*quantified-vars\*,
                  ;; this variable is unquantified. Don't include generated
                  ;; variables in this analysis
                  (unless (member name *quantified-vars* :test #'equal)
                    (pushnew name unquantified-vars :test #'equal)))
                 (_ (transform-ast formula
                                   (lambda (term &key &allow-other-keys) term)
                                   :propagator #'checker)))))
      (match original-formula
        ((ruleml-forall :vars forall-vars :clause formula)
         ;; set \"*quantified-vars*\* to the list of universally
         ;; quantified variables, and use checker to traverse
         ;; \"original-formula\"
         (setf *quantified-vars* (mapcar #'ruleml-var-name forall-vars))
         (checker formula))
        (_
         ;; unless \"original-formula\" is ground, warn that it should
         ;; be enclosed in a Forall
         (unless (ground-atom-p original-formula)
           (warn "\"Forall\" wrapper is missing from clause: ~%~A"
                 original-formula))
         (checker original-formula)))
      (when unquantified-vars
        ;; identify the unquantified variables alongside their containing
        ;; formula in a warning if any were found
        (warn "Variable~P not explicitly quantified: ~{?~A~^, ~} in the clause: ~%~A"
              (if (rest unquantified-vars) 2 1) ;; this is for plurality printing via ~p
              (nreverse unquantified-vars)
              original-formula)))))

(defun check-naf-variables (rule)
  "Signal warnings or findings in response to variables likely not
being properly instantiated in conclusions before appearing in Naf
formulas (which must appear in conditions)."
  (let ((head-vars (hash-set #'equal))     ;; the variables of the head (positive context)
        (non-naf-vars (hash-set #'equal))) ;; the non-naf variables of the body (negative context)
    (labels ((hs-superset-p (hash-set-1 hash-set-2)
               ;; \"hash-set-1\" contains \"hash-set-2\" iff the
               ;; intersection of the two is equal to \"hash-set-2\"
               (equalp hash-set-2 (inter# hash-set-1 hash-set-2)))
             (warn-head-naf-vars-missing-from-preceding-conjunct (head-naf-vars)
               ;; warn if the conclusion variables of \"head-naf-vars\"
               ;; didn't appear in a conjunct preceding a Naf
               ;; in which they all appear
               (warn "Conclusion variable~P: ~{?~A~^, ~} do~:[es ~; ~]not occur in a conjunct ~
preceding the Naf, which should be instantiated to prevent floundering:~%~A"
                     (if (= (hash-table-count head-naf-vars) 1) 1 2)
                     (keys head-naf-vars)
                     (/= (hash-table-count head-naf-vars) 1)
                     rule))
             (finding-vars-missing-from-preceding-conjunct (naf-vars)
               ;; print a finding if the variables of \"naf-vars\"
               ;; didn't appear in a conjunct preceding a Naf
               ;; in which they all appear
               (finding "Variable~P: ~{?~A~^, ~} do~:[es ~; ~]not occur in a conjunct ~
preceding the Naf:~%~A"
                     (if (= (hash-table-count naf-vars) 1) 1 2)
                     (keys naf-vars)
                     (/= (hash-table-count naf-vars) 1)
                     rule))
             (finding-naf-contains-anonymous-var ()
               ;; lastly, print a finding if a Naf formula contains an
               ;; anonymous variable
               (finding "A Naf in the rule: ~%~A~%contains an anonymous variable"
                          rule))
             (checker (term &key naf positive negative &allow-other-keys)
               (match term
                 ((ruleml-var :name name)
                  (declare (special *naf-has-anonymous-var*)
                           (special *naf-vars*))
                  ;; record the variable named \"name\" to:
                  ;;
                  ;; 1. *naf-vars* if the variable is inside a Naf.
                  ;; 2. head-vars if the variable is inside a conclusion or fact.
                  ;; 3. non-naf-vars if the variable is inside a condition
                  ;;    and outside any Naf.
                  ;;
                  ;; Also, generated variables found during the
                  ;; parsing stage are always anonymous variables, and
                  ;; we record their presence using the
                  ;; *naf-has-anonymous-var* special variable.
                  (cond ((and naf (ruleml-genvar-p term))
                         (setf *naf-has-anonymous-var* t))
                        (naf (add# name *naf-vars*))
                        (positive (add# name head-vars))
                        (negative (add# name non-naf-vars)))
                  term)
                 ((ruleml-naf :formula formula)
                  (let ((*naf-has-anonymous-var*)
                        (*naf-vars* (hash-set #'equal)))
                    (declare (special *naf-has-anonymous-var*)
                             (special *naf-vars*))

                    ;; If checker is processing a Naf formula, we
                    ;; instantiate fresh *naf-has-anonymous-var* (a
                    ;; boolean) and *naf-vars* (a hash set) variables
                    ;; for access down the call stack.
                    (checker formula :naf t :negative negative)

                    (let ((head-naf-vars (inter# head-vars *naf-vars*)))
                      (unless (hs-superset-p non-naf-vars head-naf-vars)
                        ;; Unless the head Naf variables form a
                        ;; superset of the non-Naf variables
                        ;; (condition-side variables outside of any
                        ;; Naf formula), warn about the head Naf
                        ;; variables not instantiated prior to their
                        ;; appearance in this Naf.
                        (setf head-naf-vars (diff# head-naf-vars non-naf-vars))
                        (warn-head-naf-vars-missing-from-preceding-conjunct
                         head-naf-vars))

                      ;; Subtract the sets \"non-naf-vars\" and
                      ;; \"head-vars\" from \"*naf-vars*\".
                      (setf *naf-vars* (diff# *naf-vars* non-naf-vars)
                            *naf-vars* (diff# *naf-vars* head-vars)))

                    ;; If \"*naf-vars*\" is non-empty by this point,
                    ;; print a finding about its variables being
                    ;; missing from a conjunct prior to this Naf.
                    (unless (emptyp# *naf-vars*)
                      (finding-vars-missing-from-preceding-conjunct
                       *naf-vars*))

                    ;; Finally, if this Naf was found to contain an
                    ;; anonymous variable, print a finding about it.
                    (when *naf-has-anonymous-var*
                      (finding-naf-contains-anonymous-var))

                    term))
                 (_ (transform-ast
                     term (lambda (term &key &allow-other-keys) term)
                     :propagator (lambda (term &key positive negative &allow-other-keys)
                                   (checker term :naf naf
                                                 :positive positive
                                                 :negative negative))
                     :positive positive
                     :negative negative)))))
      (checker rule))))

(defrule rule
    (or forall-clause clause)
  (:lambda (rule)
    (check-variable-quantification rule)
    (check-naf-variables rule)
    rule))

(defrule forall-clause
    (and "Forall"
         (+ (or whitespace comment var))
         #\(
         (* (or whitespace comment))
         clause
         (* (or whitespace comment))
         #\)
         (* (or whitespace comment)))
  (:destructure (forall vars lparen ws1 clause ws2 rparen ws3 &bounds start)
    (declare (ignore forall lparen ws1 ws2 rparen ws3))
    (make-ruleml-forall :vars (remove nil vars)
                        :clause clause
                        :position start)))

(defrule clause
    (or implies head))

(defrule implies
    (and head
         (* (or whitespace comment))
         ":-"
         (* (or whitespace comment))
         formula)
  (:destructure (head ws1 implies ws2 formula &bounds start)
    (declare (ignore ws1 implies ws2))
    (make-ruleml-implies :conclusion head
                         :condition formula
                         :position start)))

(defrule head
    (or head-and head-exists atomic))

(defrule naf-formula
    (and "Naf"
         #\(
         (* (or whitespace comment))
         formula
         (* (or whitespace comment))
         #\))
  (:destructure (naf lbrack ws1 formula ws2 rbrack &bounds start)
    (declare (ignore naf lbrack ws1 ws2 rbrack))
    (make-ruleml-naf :formula formula
                     :position start)))

(defrule head-exists
    (and "Exists"
         (+ (or var whitespace))
         #\(
         (* (or whitespace comment))
         head
         (* (or whitespace comment))
         #\))
  (:destructure (exists vars lparen ws1 formula ws2 rparen &bounds start)
    (declare (ignore exists ws1 ws2 lparen rparen))
    (make-ruleml-exists :vars (remove nil vars)
                        :formula formula
                        :position start)))

(defrule head-and
    (and "And"
         (* (or whitespace comment))
         #\(
         (* (or head whitespace))
         #\)
         (* (or whitespace comment)))
  (:destructure (and ws1 lparen formulas rparen ws2 &bounds start)
    (declare (ignore and ws1 lparen rparen ws2))
    (make-ruleml-and :terms (remove nil formulas)
                     :position start)))

(defrule profile
    angle-bracket-iri)

(defrule angle-bracket-iri
    iri-ref)

;; The condition language.

(defrule formula
    (or and-formula
        or-formula
        naf-formula
        exists-formula
        external-formula
        atomic))

(defrule formula-list
    (* (or formula whitespace comment))
  (:lambda (formulas)
    (remove nil formulas)))

(defrule and-formula
    (and "And" (* (or whitespace comment)) #\( formula-list #\))
  (:destructure (and ws1 lparen formulas rparen &bounds start)
    (declare (ignore and ws1 lparen rparen))
    (make-ruleml-and :terms formulas :position start)))

(defrule or-formula
    (and "Or" (* (or whitespace comment)) #\( (* (or formula whitespace comment)) #\))
  (:destructure (or ws1 lparen formulas rparen &bounds start)
    (declare (ignore or ws1 lparen rparen))
    (make-ruleml-or :terms (remove nil formulas) :position start)))

(defrule exists-formula
    (and "Exists"
         (+ (or var whitespace))
         (* (or whitespace comment))
         #\(
         (* (or whitespace comment))
         formula
         (* (or whitespace comment))
         #\))
  (:destructure (exists vars ws1 lparen ws2 formula ws3 rparen &bounds start)
    (declare (ignore exists ws1 ws2 lparen ws3 rparen))
    (make-ruleml-exists :vars (remove nil vars)
                        :formula formula
                                    :position start)))

(defrule external-formula
    (and "External"
         (* (or whitespace comment))
         #\(
         (* (or whitespace comment))
         term
         (* (or whitespace comment))
         #\))
  (:destructure (external ws1 lparen ws2 atom ws3 rparen &bounds start)
    (declare (ignore external ws1 lparen ws2 ws3 rparen))
    (make-ruleml-external :atom atom :position start)))

(defrule atomic
    (or subclass equal atom))

(defrule atom
    (or atom-oidful atom-oidless))

(defrule equal
    (and term
         (* (or whitespace comment))
         #\=
         (* (or whitespace comment))
         term)
  (:destructure (left ws1 equal ws2 right &bounds start)
    (declare (ignore ws1 equal ws2))
    (make-ruleml-equal :left left :right right :position start)))

(defrule subclass
    (and term
         (* (or whitespace comment))
         "##"
         (* (or whitespace comment))
         term)
  (:destructure (subclass ws1 subclass-rel ws2 superclass &bounds start)
    (declare (ignore ws1 subclass-rel ws2))
    (make-ruleml-subclass-rel :sub subclass :super superclass :position start)))

(defrule psoa
    (or atom expr))

(defrule atom-oidful
    (and subterm
         #\#
         (! (or #\# whitespace comment))
         atom-oidless-short)
  (:destructure (oid hash not-hash atom &bounds start)
    (declare (ignore hash not-hash))
    (if (ruleml-atom-p atom)
        (make-ruleml-oidful-atom :oid oid :predicate atom :position start)
        (make-ruleml-membership :oid oid :predicate atom :position start))))

(defrule atom-oidless
    (or atom-oidless-long atom-oidless-short))


(defrule atom-oidless-long
    (and #\#
         (! (or #\# whitespace comment))
         atom-oidless-short)
  (:destructure (hash not-hash atom &bounds start)
    (declare (ignore hash not-hash))
    (if (or (ruleml-atom-p atom)
            (ruleml-oidful-atom-p atom)
            (ruleml-membership-p atom)
            (ruleml-expr-p atom))
        atom
        (make-ruleml-atom :root atom
                          :descriptors nil
                          :position start))))

(defrule atom-oidless-short
    (and term (! #\#) (? psoa-rest))
  (:destructure (root not-hash descriptors &bounds start)
    (declare (ignore not-hash))
    #|
    atom-oidless-short must distinguish these cases
    at the type level:

    p    <-- a const (ruleml-const)
    p()  <-- an atom (ruleml-atom)

    either can be parsed by atom-oidless-short.

    in both cases, descriptors will be bound to nil / ().

    question: if descriptors is nil in either case, how do we know to
    wrap root in a ruleml-const struct or a ruleml-atom struct?

    answer: if descriptors, the value parsed by the subexpression (?
    psoa-rest), returns t, then "p()" was parsed, indicating we should
    construct a ruleml-atom.
    (see the definition of psoa-rest).

    otherwise, descriptors is (), indicating we should construct a
    ruleml-const.
    |#
    (cond ((null descriptors) root)
          ((eql descriptors t)
           (if (or (ruleml-atom-p root)
                   (ruleml-oidful-atom-p root)
                   (ruleml-membership-p root)
                   (ruleml-expr-p root))
               root
               (make-ruleml-atom :root root
                                 :position start)))
          (t (make-ruleml-atom :root root
                               :descriptors descriptors
                               :position start)))))

(define-condition implicit-and-explicit-tuples-in-same-atom (error)
  ((descriptors-position :initarg :descriptors-position)
   (implicit-term-position :initarg :implicit-term-position)
   (first-explicit-tuple-position :initarg :first-explicit-tuple-position))
  (:report (lambda (condition stream)
             (with-slots (descriptors-position
                          implicit-term-position
                          first-explicit-tuple-position)
                 condition
               (format stream
                       "The descriptor list at position ~D contains an explicit tuple at position ~D~
 occurring before an implicit tuple term at position ~D"
                       descriptors-position
                       first-explicit-tuple-position
                       implicit-term-position)))))

(define-condition not-in-left-implicit-tuple-normal-form (error)
  ((position :initarg :position)))

(defun check-implicit-tuple-terms (descriptors position)
  "Package any implicit terms as a single dependent tuple, if any
exist. Raise an error if \"descriptors\" has implicit tuple terms and
is not in left-implicit-tuple normal form (LITNF). Raise an error if
\"descriptors\" has both implicit and explicit tuples."
  (macrolet ((set-position-if-null (place descriptor)
               ;; set the NIL-valued variable ,place to the position
               ;; of the AST node ,descriptor.
               `(when (null ,place)
                  (setf ,place (ruleml-ast-node-position ,descriptor)))))
    (let* (first-slot-position
           first-tuple-position
           last-term-position
           (term-count 0)
           (terms
             (loop for descriptor in descriptors
                   if (ruleml-slot-p descriptor)
                     ;; if descriptor is the first slot to appear in
                     ;; the descriptor list, set first-slot-position
                     ;; accordingly.
                     do (set-position-if-null first-slot-position descriptor)
                   else if (ruleml-tuple-p descriptor)
                     ;; similarly if descriptor is a tuple.
                     do (set-position-if-null first-tuple-position descriptor)
                   else
                     ;; if descriptor isn't a slot or a tuple,
                     ;; collect it into the list of implicit tuple
                     ;; terms bound to \"terms\"...
                     collect descriptor into terms
                     ;; ... and increment term-count, and record
                     ;; the position of \"term\" in case it's the
                     ;; last implicit tuple term in \"descriptors\".
                     and do (incf term-count)
                            (setf last-term-position (ruleml-ast-node-position descriptor))
                   finally (return terms))))
      (cond ((null terms) descriptors)
            ((null first-tuple-position)
             ;; The atom is not in LITNF iff the first slot precedes
             ;; the last implicit tuple term. This is only checked if
             ;; no explicit tuples were found in \"descriptors\".
             (if (and (integerp first-slot-position)
                      (< first-slot-position last-term-position))
                 (error 'not-in-left-implicit-tuple-normal-form
                        :position position)
                 (cons (make-ruleml-tuple :position position
                                          :dep t
                                          :terms terms)
                       (subseq descriptors term-count))))
            (t (error 'implicit-and-explicit-tuples-in-same-atom
                      :descriptors-position position
                      :implicit-term-position last-term-position
                      :first-explicit-tuple-position first-tuple-position))))))


(defrule psoa-rest
    (and #\(
         (* (or whitespace comment))
         (? psoa-rest-list) ;; (* (or whitespace slot-di tuple-di subterm))
         (* (or whitespace comment))
         #\))
  (:destructure (lparen ws1 descriptors ws2 rparen &bounds start)
    (declare (ignore ws1 lparen ws2 rparen))
    #|
    If descriptors is empty, inform atom-oidless-short that
    psoa-rest was triggered by returning t, signifying that
    atom-oidless-short must construct an atom, despite descriptors
    being ().

    If on the other hand psoa-rest was not triggered,
    atom-oidless-short will know by finding descriptors to be NIL.
    |#
    (let ((descriptors (check-implicit-tuple-terms (remove nil descriptors) (1+ start))))
      (if (null descriptors)
          t
          descriptors))))

(defrule psoa-rest-list
    (and (or slot-di tuple-di subterm)
         (* (or whitespace comment))
         (or (& ")") psoa-rest-list))
  (:destructure (subterm ws rest)
    (declare (ignore ws))
    (if (listp rest)
        (cons subterm (remove nil rest))
        (list subterm))))

(defrule expr
    (or expr-long expr-short))

(defrule expr-short
    (and (or const subterm)
         #\(
	     (* (or whitespace comment))
         (or (+ (and subterm (! "+>") (* (or whitespace comment))))
             (* (and tuple-d (* (or whitespace comment)))))
         (* (and slot-d (* (or whitespace comment))))
         #\))
  (:destructure (root lparen ws tuple-terms slot-terms rparen &bounds start)
    (declare (ignore lparen ws rparen))
    (let* ((remove-nil-from-sublists (alexandria:compose #'first (alexandria:curry #'remove nil)))
           (tuple-terms (mapcar remove-nil-from-sublists tuple-terms))
           (slot-terms  (mapcar remove-nil-from-sublists slot-terms)))
      (make-ruleml-expr :root root
                        :terms (append tuple-terms slot-terms)
                        :position start))))

(defrule expr-long
    (and #\^ expr-short)
  (:destructure (hat expr)
    (declare (ignore hat))
    expr))

(defrule tuple-di
    (and (or plus-sign minus-sign)
         #\[
         (* (or subterm whitespace comment))
         #\])
  (:destructure (dep lsqbrack terms rsqbrack &bounds start)
    (declare (ignore lsqbrack rsqbrack))
    (let ((dep (string= dep "+")))
      (make-ruleml-tuple :dep dep
                         :terms (remove nil terms)
                         :position start))))

(defrule tuple-d
    (and plus-sign
         #\[
         (* (or subterm whitespace comment))
         #\])
  (:destructure (plus-sign lsqbrack terms rsqbrack &bounds start)
    (declare (ignore plus-sign lsqbrack rsqbrack))
    (make-ruleml-tuple :dep t
                       :terms (remove nil terms)
                       :position start)))

(defrule slot-di
    (and subterm (or "+>" "->") subterm)
  (:destructure (name dep filler &bounds start)
    (let ((dep (string= dep "+>")))
      (make-ruleml-slot :dep dep :name name :filler filler :position start))))

(defrule slot-d
    (and subterm "+>" subterm)
  (:destructure (term1 dep-sign term2 &bounds start)
    (declare (ignore dep-sign))
    (make-ruleml-slot :dep t :name term1 :filler term2 :position start)))

(defrule term
    (or external-expr
        atom
        const
        var
        expr))

(defrule subterm
    (or external-expr
        atom-oidful
        expr
        atom-oidless-long
        const
        var))

(defrule external-expr
    (and "External"
	     (* (or whitespace comment))
	     #\(
	     (* (or whitespace comment))
	     expr
	     (* (or whitespace comment))
	     #\)
	     (* (or whitespace comment)))
  (:destructure (external ws1 lparen ws2 expr ws3 rparen ws4 &bounds start)
    (declare (ignore external ws1 lparen rparen ws2 ws3 ws4))
    (make-ruleml-external :atom expr :position start)))

(defrule const
    (or const-short const-string))

(defrule const-short
    (or angle-bracket-iri
        curie
        numeric-literal
        pn-local ;; (and #\_ (? pn-local))
        (and unicode-string (! "^^")))
  (:lambda (const &bounds start)
    (typecase const
      (ruleml-string const)
      (ruleml-const const)
      (ruleml-iri const)
      (list (car (remove nil const)))
      (number (make-ruleml-number :value const :position start))
      (t (make-ruleml-const :contents const :position start)))))

(defrule const-string
    (and unicode-string "^^" sym-space)
  (:destructure (unicode-string carets sym-space &bounds start)
    (declare (ignore carets))
    (let ((operand (ruleml-string-contents unicode-string)))
      (typecase sym-space
        (ruleml-iri
         ;; try to perform a cast of the string \"operand\" to the type
         ;; described by \"sym-space\", a ruleml-iri. if we cannot resolve
         ;; to a pre-defined cast, create a ruleml-expr named \"sym-space"
         ;; with \"unicode-string\" as its sole argument.
         (let ((value (prefix-type-cast (ruleml-iri-contents sym-space) operand)))
           (if value
               value
               #1=(make-ruleml-expr
                   :root sym-space
                   :terms (list (make-ruleml-tuple :dep t :terms (list unicode-string)))
                   :position start))))
        (ruleml-pname-ln
         (let ((ns (ruleml-pname-ln-name sym-space))
               (local (ruleml-pname-ln-url sym-space)))
           ;; again try to perform a cast of the string \"operand\" according
           ;; to the type described by \"sym-space\", but this time, resolve
           ;; \"sym-space\" through the hash table dynamically bound to \"*prefix-ht*\".
           ;; if that fails, again evaluate the form bound to #1#.
           (declare (special *prefix-ht*))
           (multiple-value-bind (url foundp)
               (gethash ns *prefix-ht*)
             (if foundp
                 (let* ((cast (format nil "~A~A" (ruleml-iri-contents url) local))
                        (value (prefix-type-cast cast operand)))
                   (if value
                       value
                       #1#))
                 #1#))))))))

(defrule sym-space
    (or angle-bracket-iri curie))

(defrule var
    (and #\? (? pn-local))
  (:destructure (question-mark pn-local &bounds start)
    (declare (ignore question-mark))
    (if pn-local
        (make-ruleml-var :name pn-local :position start)
        (psoa-transformers::fresh-variable))))


;; Lexical analysis.

(defrule unicode-string
    (and #\" (* (or echar (not (or #\" #\\ eol)))) #\")
  (:destructure (dbl-quote0 chars dbl-quote1 &bounds start)
    (declare (ignore dbl-quote0 dbl-quote1))
    (make-ruleml-string :contents (concatenate 'string chars)
                        :position start)))

(defrule iri-ref
    (and #\< (* iri-ref-char) #\>)
  (:destructure (lbrack iri-ref-chars rbrack &bounds start)
    (declare (ignore lbrack rbrack))
    (make-ruleml-iri :contents (concatenate 'string iri-ref-chars)
                     :position start)))

(defrule iri-ref-char
    (not (or #\< #\> #\" #\{ #\} #\| #\^ #\` #\\
             (character-ranges (#\u0000 #\u0020)))))

(defrule pn-local
    (and (or pn-chars-u
             digit
             plx)
         (* (or pn-chars plx)))
  (:text t))

(defrule pn-chars
    (or pn-chars-u
        minus-sign
        digit
        #\u00b7
        (character-ranges (#\u0300 #\u036f))
        (character-ranges (#\u203f #\u2040))))

(defrule pn-chars-u
    (or pn-chars-base #\_))

(defrule pn-chars-base
    (or alpha
        (character-ranges (#\u00C0 #\u00D6))
        (character-ranges (#\u00d8 #\u00f6))
        (character-ranges (#\u00f8 #\u02ff))
        (character-ranges (#\u0370 #\u037d))
        (character-ranges (#\u037f #\u1fff))
        (character-ranges (#\u200c #\u200d))
        (character-ranges (#\u2070 #\u218f))
        (character-ranges (#\u2c00 #\u2fef))
        (character-ranges (#\u3001 #\ud7ff))
        (character-ranges (#\uf900 #\ufdcf))
        (character-ranges (#\ufdf0 #\ufffd))))

(defrule plx
    (or percent pn-local-esc))

(defrule percent
    (and #\% hex hex)
  (:text t))

(defrule hex
    (or digit
        (character-ranges (#\a #\f))
        (character-ranges (#\A #\F))))

(defrule pn-local-esc
    (and #\\
         (or #\_ #\~ #\. #\! #\$ #\& #\* #\,
             #\; #\= #\: #\/ #\? #\( #\) #\#
             plus-sign minus-sign
             #\@ #\%))
  (:text t))

(defrule alpha
    (alpha-char-p character))

(defrule digit
    (digit-char-p character))

(defrule echar
    (and #\\ (character-ranges #\t #\b #\n #\r #\f #\\ #\" #\'))
  (:destructure (backslash char)
    (declare (ignore backslash))
    char))

(defrule eol
    (or #\newline #\return))

;; https://www.w3.org/TR/2010/NOTE-curie-20101216/
(defrule curie
    (or pname-ln pname-ns))

(defrule pname-ln
    (and pname-ns pn-local)
  (:destructure (ns local &bounds start)
    (make-ruleml-pname-ln :name ns :url local :position start)))

(defrule pname-ns
    (and (? pn-prefix) #\:)
  (:destructure (pn-prefix colon)
    (declare (ignore colon))
    pn-prefix))

(defrule name nc-name)

(defrule pn-prefix ;; PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?
    (and pn-chars-base (? (* (or (and (+ #\.) pn-chars) pn-chars))))
  (:text t))

(defrule nc-name
    (and (or alpha #\_) (* nc-name-char))
  (:text t))

(defrule nc-name-char
    (or alpha digit #\. minus-sign #\_ combining-char extender))

(defrule combining-char
    (or (character-ranges (#\combining_grave_accent
                           #\combining_greek_ypogegrammeni))
        (character-ranges (#\combining_double_tilde
                           #\combining_double_inverted_breve))
        (character-ranges (#\combining_cyrillic_titlo
                           #\combining_cyrillic_psili_pneumata))
        (character-ranges (#\hebrew_accent_etnahta #\hebrew_accent_pazer))
        (character-ranges (#\hebrew_accent_munah #\hebrew_point_holam))
        (character-ranges (#\hebrew_point_qubuts #\hebrew_point_meteg))
        #\hebrew_point_rafe
        (character-ranges (#\hebrew_point_shin_dot #\hebrew_point_sin_dot))
        #\hebrew_mark_upper_dot
        (character-ranges (#\arabic_fathatan #\arabic_sukun))
        #\arabic_letter_superscript_alef
        (character-ranges
         (#\arabic_small_high_ligature_sad_with_lam_with_alef_maksura
          #\arabic_small_high_seen))
        (character-ranges (#\arabic_end_of_ayah #\arabic_small_high_rounded_zero))
        (character-ranges (#\arabic_small_high_upright_rectangular_zero
                           #\arabic_small_high_madda))
        (character-ranges (#\arabic_small_high_yeh #\arabic_small_high_noon))
        (character-ranges (#\arabic_empty_centre_low_stop #\arabic_small_low_meem))
        (character-ranges (#\devanagari_sign_candrabindu #\devanagari_sign_visarga))
        #\devanagari_sign_nukta
        (character-ranges (#\devanagari_vowel_sign_aa #\devanagari_vowel_sign_au))
        #\devanagari_sign_virama
        (character-ranges (#\devanagari_stress_sign_udatta
                           #\devanagari_acute_accent))
        (character-ranges (#\devanagari_vowel_sign_vocalic_l
                           #\devanagari_vowel_sign_vocalic_ll))
        (character-ranges (#\bengali_sign_candrabindu #\bengali_sign_visarga))
        #\bengali_sign_nukta #\bengali_vowel_sign_aa #\bengali_vowel_sign_i
        (character-ranges (#\bengali_vowel_sign_ii #\bengali_vowel_sign_vocalic_rr))
        (character-ranges (#\bengali_vowel_sign_e #\bengali_vowel_sign_ai))
        (character-ranges (#\bengali_vowel_sign_o #\bengali_sign_virama))
        #\bengali_au_length_mark
        (character-ranges (#\bengali_vowel_sign_vocalic_l
                           #\bengali_vowel_sign_vocalic_ll))
        #\gurmukhi_sign_bindi #\gurmukhi_sign_nukta #\gurmukhi_vowel_sign_aa
        #\gurmukhi_vowel_sign_i
        (character-ranges (#\gurmukhi_vowel_sign_ii #\gurmukhi_vowel_sign_uu))
        (character-ranges (#\gurmukhi_vowel_sign_ee #\gurmukhi_vowel_sign_ai))
        (character-ranges (#\gurmukhi_vowel_sign_oo #\gurmukhi_sign_virama))
        (character-ranges (#\gurmukhi_tippi #\gurmukhi_addak))
        (character-ranges (#\gujarati_sign_candrabindu #\gujarati_sign_visarga))
        #\gujarati_sign_nukta
        (character-ranges (#\gujarati_vowel_sign_aa #\gujarati_vowel_sign_candra_e))
        (character-ranges (#\gujarati_vowel_sign_e #\gujarati_vowel_sign_candra_o))
        (character-ranges (#\gujarati_vowel_sign_o #\gujarati_sign_virama))
        (character-ranges (#\oriya_sign_candrabindu #\oriya_sign_visarga))
        #\oriya_sign_nukta
        (character-ranges (#\oriya_vowel_sign_aa #\oriya_vowel_sign_vocalic_r))
        (character-ranges (#\oriya_vowel_sign_e #\oriya_vowel_sign_ai))
        (character-ranges (#\oriya_vowel_sign_o #\oriya_sign_virama))
        (character-ranges (#\oriya_ai_length_mark #\oriya_au_length_mark))
        (character-ranges (#\tamil_sign_anusvara #\tamil_sign_visarga))
        (character-ranges (#\tamil_vowel_sign_aa #\tamil_vowel_sign_uu))
        (character-ranges (#\tamil_vowel_sign_e #\tamil_vowel_sign_ai))
        (character-ranges (#\tamil_vowel_sign_o #\tamil_sign_virama))
        #\tamil_au_length_mark
        (character-ranges (#\telugu_sign_candrabindu #\telugu_sign_visarga))
        (character-ranges (#\telugu_vowel_sign_aa #\telugu_vowel_sign_vocalic_rr))
        (character-ranges (#\telugu_vowel_sign_e #\telugu_vowel_sign_ai))
        (character-ranges (#\telugu_vowel_sign_o #\telugu_sign_virama))
        (character-ranges (#\telugu_length_mark #\telugu_ai_length_mark))
        (character-ranges (#\kannada_sign_anusvara #\kannada_sign_visarga))
        (character-ranges (#\kannada_vowel_sign_aa #\kannada_vowel_sign_vocalic_rr))
        (character-ranges (#\kannada_vowel_sign_e #\kannada_vowel_sign_ai))
        (character-ranges (#\kannada_vowel_sign_o #\kannada_sign_virama))
        (character-ranges (#\kannada_length_mark #\kannada_ai_length_mark))
        (character-ranges (#\malayalam_sign_anusvara #\malayalam_sign_visarga))
        (character-ranges (#\malayalam_vowel_sign_aa
                           #\malayalam_vowel_sign_vocalic_r))
        (character-ranges (#\malayalam_vowel_sign_e #\malayalam_vowel_sign_ai))
        (character-ranges (#\malayalam_vowel_sign_o #\malayalam_sign_virama))
        #\malayalam_au_length_mark #\thai_character_mai_han-akat
        (character-ranges (#\thai_character_sara_i #\thai_character_phinthu))
        (character-ranges (#\thai_character_maitaikhu #\thai_character_yamakkan))
        #\lao_vowel_sign_mai_kan
        (character-ranges (#\lao_vowel_sign_i #\lao_vowel_sign_uu))
        (character-ranges (#\lao_vowel_sign_mai_kon #\lao_semivowel_sign_lo))
        (character-ranges (#\lao_tone_mai_ek #\lao_niggahita))
        (character-ranges (#\tibetan_astrological_sign_-khyud_pa
                           #\tibetan_astrological_sign_sdong_tshugs))
        #\tibetan_mark_ngas_bzung_nyi_zla #\tibetan_mark_ngas_bzung_sgor_rtags
        #\tibetan_mark_tsa_-phru #\tibetan_sign_yar_tshes #\tibetan_sign_mar_tshes
        (character-ranges (#\tibetan_vowel_sign_aa #\tibetan_mark_halanta))
        (character-ranges (#\tibetan_sign_lci_rtags #\tibetan_sign_gru_med_rgyings))
        (character-ranges (#\tibetan_subjoined_letter_ka
                           #\tibetan_subjoined_letter_ca))
        #\tibetan_subjoined_letter_ja
        (character-ranges (#\tibetan_subjoined_letter_nya
                           #\tibetan_subjoined_letter_wa))
        (character-ranges (#\tibetan_subjoined_letter_ya
                           #\tibetan_subjoined_letter_ha))
        #\tibetan_subjoined_letter_kssa
        (character-ranges (#\combining_left_harpoon_above
                           #\combining_four_dots_above))
        #\combining_left_right_arrow_above
        (character-ranges (#\ideographic_level_tone_mark
                           #\hangul_double_dot_tone_mark))
        #\combining_katakana-hiragana_voiced_sound_mark
        #\combining_katakana-hiragana_semi-voiced_sound_mark))

(defrule extender
    (or #\u00B7 #\u02D0 #\u02D1 #\u0387 #\u0640 #\u0E46 #\u0EC6 #\u3005
        (character-ranges (#\u3031 #\u3035))
        (character-ranges (#\u309D #\u309E))
        (character-ranges (#\u30FC #\u30FE))))

(defrule numeric-literal
    (and (or numeric-literal-positive
             numeric-literal-negative
             numeric-literal-unsigned)
         (! (or pn-chars plx)) ;; don't mistake the prefix of a
                               ;; pn-local for a standalone
                               ;; numeric-literal
         )
  (:destructure (numeric-literal trailing-nil)
    (declare (ignore trailing-nil))
    numeric-literal))

(defrule numeric-literal-unsigned
    (or double decimal integer))

(defrule numeric-literal-positive
    (or double-positive decimal-positive integer-positive))

(defrule numeric-literal-negative
    (or double-negative decimal-negative integer-negative))

(defrule integer
    (+ digit)
  (:text t)
  (:function parse-integer))

(defrule decimal
    (or (and (+ digit) #\. (* digit))
        (and #\. (+ digit)))
  (:text t)
  (:function read-from-string))

(defrule double
    (or (and (+ digit) #\. (* digit) exponent)
        (and #\. (+ digit) exponent)
        (and (+ digit) exponent))
  (:text t)
  (:function read-from-string))

(defrule exponent
    (and (or #\e #\E) (? (or plus-sign minus-sign)) (+ digit))
  (:text t))

(defrule integer-positive
    (and plus-sign (& digit) integer)
  (:destructure (minus-sign lookahead integer)
    (declare (ignore minus-sign lookahead))
    integer))

(defrule integer-negative
    (and minus-sign (& digit) integer)
  (:destructure (minus-sign lookahead integer)
    (declare (ignore minus-sign lookahead))
    (- integer)))

(defrule decimal-positive
    (and plus-sign (& (or digit #\.)) decimal)
  (:destructure (plus-sign lookahead decimal)
    (declare (ignore plus-sign lookahead))
    decimal))

(defrule decimal-negative
    (and minus-sign (& (or digit #\.)) decimal)
  (:destructure (minus-sign lookahead decimal)
    (declare (ignore minus-sign lookahead))
    (- decimal)))

(defrule double-positive
    (and plus-sign (& (or digit #\.)) double)
  (:destructure (plus-sign lookahead double)
    (declare (ignore plus-sign lookahead))
    double))

(defrule double-negative
    (and minus-sign (& (or digit #\.)) double)
  (:destructure (minus-sign lookahead double)
    (declare (ignore minus-sign lookahead))
    (- double)))

(defrule whitespace
    (+ (or #\Space #\Tab #\Newline #\Return (and #\Return #\Newline)))
  (:constant nil))

(defrule plus-sign
    (and (! "+>") #\+)
  (:text t))

(defrule minus-sign
    (and (! "->") #\-)
  (:text t))
