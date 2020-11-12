
(in-package #:prolog-grammar)

#|

A simplified Prolog parser written in esrap rules representing Prolog
goals and equalities in terms of the ruleml-ast-node subtyped structs
of #:psoa-ast. The Prolog server returns solution strings in Prolog
notation, which are parsed on the Lisp side as PSOA RuleML ASTs, and
printed back to the REPL and to the test suite, using the PSOA RuleML
pretty printer defined in #:psoa-pprint.

|#

(defrule whitespace
    (+ (or #\Space #\Tab #\Newline (and #\Return #\Newline)))
  (:constant nil))

(defrule goal-sequence
    (and goal
         (* whitespace)
         (? (and #\, (* whitespace) goal-sequence))
         (* whitespace))
  (:destructure (goal ws1 goals ws2)
    (declare (ignore ws1 ws2))
    (if goals
        (psoa-transformers::flatten-and (make-ruleml-and :terms (cons goal (cddr goals))))
        (list goal))))

(defrule goal
    (or equals-goal functor-goal))

(defrule equals-goal
    (and (or "=(" "'='(") arg "," (* whitespace) arg ")")
  (:destructure (equals left comma ws right rparen)
    (declare (ignore equals comma ws rparen))
    (make-ruleml-equal :left left :right right)))

(defrule functor-goal
    (and prolog-symbol (? (and #\( arg-list #\))))
  (:destructure (const arg-list)
    (if arg-list
        (make-ruleml-expr :root const :terms (cadr arg-list))
        const)))

(defrule arg-list
    (and arg
         (* whitespace)
         (? (and #\, (* whitespace) arg-list)))
  (:destructure (arg ws arg-list)
    (declare (ignore ws))
    (if arg-list
        (cons arg (caddr arg-list))
        (list arg))))

(defrule prolog-symbol
    (or psoa-grammar::const-short quoted-prolog-symbol standard-prolog-symbol))

(defrule string
    psoa-grammar::unicode-string)

(defrule numeric-literal
    psoa-grammar::numeric-literal
  (:lambda (number)
    (make-ruleml-number :value number)))

(defrule standard-prolog-symbol
    (and (character-ranges (#\a #\z))
         psoa-grammar::pn-local)
  (:text t)
  (:lambda (contents)
    (make-ruleml-const :contents contents)))

(defrule quoted-prolog-symbol
    (and #\'
         (or psoa-grammar::const-short standard-prolog-symbol)
         #\')
  (:destructure (q1 const q2)
    (declare (ignore q1 q2))
    const))

(defrule arg
    (or var goal))

(defrule var
    (and (or #\_ (character-ranges (#\A #\Z)))
         (* (not (or #\) #\,))))
  (:text t)
  (:lambda (contents)
    (make-ruleml-var :name (subseq contents 1)))) ;; Skip the leading 'Q' or '_'.
