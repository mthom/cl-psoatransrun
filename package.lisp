;;;; package.lisp

(require :esrap)
(require :rutils)
(require :trivia)

(defpackage #:psoa-ast
  (:use #:cl #:trivia)
  (:export #:walk-ast
           #:ruleml
           #:ruleml-document
           #:ruleml-document-base
           #:ruleml-document-prefixes
           #:ruleml-document-imports
           #:ruleml-document-performatives
           #:ruleml-base
           #:ruleml-base-iri-ref
           #:ruleml-prefix
           #:ruleml-prefix-name
           #:ruleml-prefix-iri-ref
           #:ruleml-import
           #:ruleml-import-iri-ref
           #:ruleml-import-profile
           #:ruleml-assert
           #:ruleml-assert-items
           #:ruleml-query
           #:ruleml-query-term
           #:ruleml-naf
           #:ruleml-naf-formula
           #:ruleml-var
           #:ruleml-var-name
           #:ruleml-slot
           #:ruleml-slot-dep
           #:ruleml-slot-name
           #:ruleml-slot-filler
           #:ruleml-tuple
           #:ruleml-tuple-dep
           #:ruleml-tuple-terms
           #:ruleml-pname-ln
           #:ruleml-pname-ln-name
           #:ruleml-pname-ln-url
           #:ruleml-expr
           #:ruleml-expr-root
           #:ruleml-expr-terms
           #:ruleml-subclass-rel
           #:ruleml-subclass-rel-sub
           #:ruleml-subclass-rel-super
           #:ruleml-external
           #:ruleml-external-atom
           #:ruleml-equal
           #:ruleml-equal-left
           #:ruleml-equal-right
           #:ruleml-atom
           #:ruleml-atom-root
           #:ruleml-atom-descriptors
           #:ruleml-oidful-atom
           #:ruleml-oidful-atom-oid
           #:ruleml-oidful-atom-predicate
           #:ruleml-forall
           #:ruleml-forall-vars
           #:ruleml-forall-clause
           #:ruleml-query
           #:ruleml-query-term
           #:ruleml-implies
           #:ruleml-implies-conclusion
           #:ruleml-implies-condition
           #:ruleml-and
           #:ruleml-and-terms
           #:ruleml-or
           #:ruleml-or-terms
           #:ruleml-exists
           #:ruleml-exists-vars
           #:ruleml-exists-formula
           #:ruleml-const
           #:ruleml-const-contents
           #:ruleml-string
           #:ruleml-string-contents
           #:ruleml-membership
           #:ruleml-membership-oid
           #:ruleml-membership-predicate
           #:ruleml-const-p
           #:ruleml-string-p
           #:ruleml-external-p
           #:ruleml-document-p
           #:ruleml-base-p
           #:ruleml-prefix-p
           #:ruleml-import-p
           #:ruleml-assert-p
           #:ruleml-query-p
           #:ruleml-naf-p
           #:ruleml-var-p
           #:ruleml-string-p
           #:ruleml-slot-p
           #:ruleml-tuple-p
           #:ruleml-pname-ln-p
           #:ruleml-expr-p
           #:ruleml-subclass-rel-p
           #:ruleml-equal-p
           #:ruleml-atom-p
           #:ruleml-oidful-atom-p
           #:ruleml-forall-p
           #:ruleml-query-p
           #:ruleml-implies-p
           #:ruleml-and-p
           #:ruleml-or-p
           #:ruleml-exists-p
           #:ruleml-external-p
           #:ruleml-membership-p
           #:ruleml-ast-node-position
           #:make-ruleml-document
           #:make-ruleml-const
           #:make-ruleml-base
           #:make-ruleml-prefix
           #:make-ruleml-import
           #:make-ruleml-assert
           #:make-ruleml-query
           #:make-ruleml-naf
           #:make-ruleml-var
           #:make-ruleml-slot
           #:make-ruleml-tuple
           #:make-ruleml-pname-ln
           #:make-ruleml-expr
           #:make-ruleml-subclass-rel
           #:make-ruleml-equal
           #:make-ruleml-atom
           #:make-ruleml-oidful-atom
           #:make-ruleml-forall
           #:make-ruleml-query
           #:make-ruleml-string
           #:make-ruleml-implies
           #:make-ruleml-and
           #:make-ruleml-or
           #:make-ruleml-exists
           #:make-ruleml-external
           #:make-ruleml-membership))

(defpackage #:psoa-pprint
  (:use #:cl #:psoa-ast #:rutils)
  (:export #:pprint-ruleml-document))

(defpackage #:psoa-grammar
  (:use #:cl #:esrap #:psoa-ast)
  (:export #:parse))

(defpackage #:psoa-transformers
  (:use #:cl #:psoa-ast #:psoa-grammar #:rutils #:trivia)
  (:export #:unnest #:objectify)
  (:shadowing-import-from :trivia trivia.level2:alist))

(defpackage #:PSOATransRun
  (:use #:cl))
