;;;; package.lisp

(require :esrap)
(require :rutils)
(require :trivia)


;; See http://clhs.lisp.se/Body/m_defpkg.htm for documentation on the
;; defpackage macro options.

(defpackage #:psoa-ast
  (:use #:cl #:rutils #:trivia)
  (:export #:transform-ast
           #:ruleml
           #:ruleml-document
           #:ruleml-document-base
           #:ruleml-document-prefixes
           #:ruleml-document-imports
           #:ruleml-document-performatives
           #:ruleml-document-prefix-ht
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
           #:ruleml-genvar
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
           #:ruleml-number
           #:ruleml-number-value
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
           #:ruleml-number-p
           #:ruleml-genvar-p
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
           #:make-ruleml-genvar
           #:make-ruleml-var
           #:make-ruleml-slot
           #:make-ruleml-tuple
           #:make-ruleml-number
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
           #:make-ruleml-membership)
  (:shadowing-import-from :trivia trivia.level2:alist TRIVIA.LEVEL2:@))


(defpackage #:psoa-pprint
  (:use #:cl #:psoa-ast #:rutils)
  (:export #:pprint-ruleml-document))

(defpackage #:psoa-grammar
  (:use #:cl #:esrap #:psoa-ast)
  (:export #:parse))

(defpackage #:psoa-transformers
  (:use #:cl #:psoa-ast #:psoa-grammar #:rutils #:trivia)
  (:export #:make-url-const #:transform-document #:transform-query
           #:*is-relational-p*  #:*static-objectification-only*)
  (:shadowing-import-from :trivia trivia.level2:alist TRIVIA.LEVEL2:@))

(defpackage #:psoa-prolog-translator
  (:use #:cl #:psoa-ast #:psoa-transformers #:rutils #:trivia)
  (:export #:translate-document #:translate-query)
  (:shadowing-import-from :trivia trivia.level2:alist TRIVIA.LEVEL2:@))

(defpackage #:prolog-engine-client
  (:use #:cl #:usocket)
  (:export #:close-socket
           #:make-engine-client
           #:open-socket-to-prolog
           #:prolog-engine-client-path
           #:prolog-engine-client-host
           #:in-socket-stream
           #:out-socket-stream))

(defpackage #:psoatransrun
  (:use #:cl #:prolog-engine-client #:psoa-ast
        #:psoa-grammar #:psoa-transformers #:psoa-prolog-translator
        #:rutils #:trivia #:usocket)
  (:export #:*all-solutions*
           #:*prolog-engine-path*
           #:connect-to-prolog-process
           #:init-prolog-process
           #:psoa-document->prolog
           #:psoa-load-and-repl
           #:psoa-query->prolog
           #:quit-prolog-engine
           #:read-and-collect-solutions
           #:send-query-to-prolog-engine
           #:start-prolog-process)
  (:shadowing-import-from :trivia trivia.level2:alist TRIVIA.LEVEL2:@))

(defpackage #:psoatransrun-tests
  (:use #:cl #:esrap #:pathname-utils #:prolog-engine-client
        #:psoa-ast #:psoatransrun #:rutils #:split-sequence #:usocket)
  (:export #:run-test-suite))

(defpackage #:prolog-grammar
  (:use #:cl #:esrap #:psoa-ast))

