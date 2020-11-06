
(in-package #:prolog-grammar)

(defrule whitespace
    (+ (or #\Space #\Tab #\Newline (and #\Return #\Newline)))
  (:constant nil))

(defrule goal-sequence
    (and goal
         (* whitespace)
         (? (and #\, (* whitespace) goal-sequence)))
  (:destructure (goal ws goals)
    (declare (ignore ws))
    (if goals
        (psoa-transformers::flatten-and (make-ruleml-and :terms (cons goal (cddr goals))))
        (list goal))))

(defrule goal
    (or equals-goal functor-goal))

(defrule equals-goal
    (and "'='(" arg "," (* whitespace) arg ")")
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
    (or quoted-prolog-symbol standard-prolog-symbol string numeric-literal))

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
         (* (not #\'))
         #\')
  (:destructure (q1 chars q2)
    (declare (ignore q1 q2))
    (make-ruleml-const :contents (concatenate 'string chars))))

(defrule arg
    (or var goal))

(defrule var
    (and (character-ranges (#\A #\Z))
         (* (not (or #\) #\,))))
  (:text t)
  (:lambda (contents)
    (make-ruleml-var :name (subseq contents 1)))) ;; Skip the leading 'Q'.
