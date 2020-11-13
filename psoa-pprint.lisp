
(in-package #:psoa-pprint)

#|
This package contains functions for pretty-printing objects whose
supertype is ruleml-ast-node, all of which are defined in
psoa-ast.lisp (i.e., the content of the package #:psoa-ast).

While objects of supertype ruleml-ast-node are assembled from string
KBs into PSOA RuleML ASTs by the EBNF-like grammar rules of
psoa-grammar.lisp, the functions here perform the inverse function:
they convert PSOA RuleML ASTs back to the string representations of
the PSOA RuleML KBs approximately like those from which they were
produced.

Documentation of the Common Lisp Pretty Printing System, which is used
extensively in this package, is available here:

https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node253.html
|#


(defparameter *inside-psoa-rest* nil
  "A contextual flag / boolean indicating whether the subterms of a
PSOA RuleML atom or expression are currently being printed.")

(defparameter *print-caret-before-expr* nil
  "A boolean indicating whether exprs outside atoms should be
preceded by caret (^).")

(defun pprint-ruleml-document (stream document)
  (pprint-logical-block (stream nil :prefix "RuleML(" :suffix ")")
    (pprint-indent :block -4 stream)
    (pprint-newline :mandatory stream)
    (when-it (ruleml-document-base document)
      (write it :stream stream)
      (pprint-newline :mandatory stream)
      (pprint-newline :mandatory stream))
    (when-it (ruleml-document-prefixes document)
      (pprint-linear stream it nil)
      (pprint-newline :mandatory stream)
      (pprint-newline :mandatory stream))
    (when-it (ruleml-document-imports document)
      (pprint-linear stream it nil)
      (pprint-newline :mandatory stream)
      (pprint-newline :mandatory stream))
    (when-it (ruleml-document-performatives document)
      (pprint-linear stream it nil))
    (pprint-indent :block (- (length "RuleML(")) stream)
    (pprint-newline :mandatory stream)))

(set-pprint-dispatch 'ruleml-document 'pprint-ruleml-document)

(defun pprint-ruleml-base (stream base)
  (pprint-logical-block (stream nil :prefix "Base(" :suffix ")")
    (write-char #\< stream)
    (write (ruleml-base-iri-ref base) :stream stream :escape nil)
    (write-char #\> stream)))

(set-pprint-dispatch 'ruleml-base 'pprint-ruleml-base)

(defun pprint-ruleml-prefix (stream prefix)
  (pprint-logical-block (stream nil :prefix "Prefix(" :suffix ")")
    (write (ruleml-prefix-name prefix) :stream stream :escape nil)
    (write-char #\: stream)
    (write-char #\< stream)
    (write (ruleml-prefix-iri-ref prefix) :stream stream :escape nil)
    (write-char #\> stream)))

(set-pprint-dispatch 'ruleml-prefix 'pprint-ruleml-prefix)

(defun pprint-ruleml-const (stream const)
  (write (ruleml-const-contents const) :stream stream :escape nil))

(set-pprint-dispatch 'ruleml-const 'pprint-ruleml-const)

(defun pprint-ruleml-string (stream string)
  (write-char #\" stream)
  (write (ruleml-string-contents string) :stream stream)
  (write-char #\" stream))

(set-pprint-dispatch 'ruleml-string 'pprint-ruleml-string)

(defun pprint-ruleml-import (stream import)
  (pprint-logical-block (stream nil :prefix "Import(" :suffix ")")
    (write-char #\< stream)
    (write (ruleml-import-iri-ref import) :stream stream)
    (write-char #\> stream)
    (pprint-newline :fill stream)
    (when-it (ruleml-import-profile import)
      (write-char #\Space stream)
      (write it :stream stream))))

(set-pprint-dispatch 'ruleml-import 'pprint-ruleml-import)

(defun pprint-ruleml-assert (stream assert)
  (pprint-logical-block (stream nil :prefix "Assert(" :suffix ")")
    (pprint-indent :block -4 stream)
    (pprint-newline :mandatory stream)
    (pprint-logical-block (stream (ruleml-assert-items assert))
      (pprint-exit-if-list-exhausted)
      (loop (write (pprint-pop) :stream stream)
            (pprint-exit-if-list-exhausted)
            (pprint-newline :mandatory stream)
            (pprint-newline :mandatory stream)))
    (pprint-indent :block (- (length "Assert(")) stream)
    (pprint-newline :mandatory stream)))

(set-pprint-dispatch 'ruleml-assert 'pprint-ruleml-assert)

(defun pprint-ruleml-query (stream query)
  (pprint-logical-block (stream nil :prefix "Query(" :suffix ")")
    (write (ruleml-query-term query) :stream stream)))

(set-pprint-dispatch 'ruleml-query 'pprint-ruleml-query)

(defun pprint-ruleml-naf (stream formula)
  (pprint-logical-block (stream nil :prefix "Naf(" :suffix ")")
    (write (ruleml-naf-formula formula) :stream stream)))

(set-pprint-dispatch 'ruleml-naf 'pprint-ruleml-naf)

(defun pprint-ruleml-var (stream var)
  (write-char #\? stream)
  (write (ruleml-var-name var) :stream stream :escape nil))

(set-pprint-dispatch 'ruleml-var 'pprint-ruleml-var)
(set-pprint-dispatch 'ruleml-genvar 'pprint-ruleml-var)

(defun pprint-ruleml-slot (stream slot)
  (write (ruleml-slot-name slot) :stream stream)
  (write-string (if (ruleml-slot-dep slot) "+>" "->") stream)
  (write (ruleml-slot-filler slot) :stream stream))

(set-pprint-dispatch 'ruleml-slot 'pprint-ruleml-slot)

(defun pprint-ruleml-tuple (stream tuple)
  (write-string (if (ruleml-tuple-dep tuple) "+" "-") stream)
  (write-char #\[ stream)
  (pprint-fill stream (ruleml-tuple-terms tuple) nil)
  (write-char #\] stream))

(set-pprint-dispatch 'ruleml-tuple 'pprint-ruleml-tuple)

(defun pprint-ruleml-pname-ln (stream pname-ln)
  (when-it (ruleml-pname-ln-name pname-ln)
    (write it :stream stream))
  (write-char #\: stream)
  (write (ruleml-pname-ln-url pname-ln) :stream stream))

(set-pprint-dispatch 'ruleml-pname-ln 'pprint-ruleml-pname-ln)

(defun pprint-ruleml-expr (stream expr)
  (unless (or *inside-psoa-rest* (not *print-caret-before-expr*))
    (write-char #\^ stream))
  (write (ruleml-expr-root expr) :stream stream)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (let ((*inside-psoa-rest* t))
      (pprint-fill stream (ruleml-expr-terms expr) nil))))

(set-pprint-dispatch 'ruleml-expr 'pprint-ruleml-expr)

(defun pprint-ruleml-subclass-rel (stream subclass-rel)
  (write (ruleml-subclass-rel-sub subclass-rel) :stream stream)
  (pprint-newline :fill stream)
  (write-string "##" stream)
  (pprint-newline :fill stream)
  (write (ruleml-subclass-rel-super subclass-rel) :stream stream))

(set-pprint-dispatch 'ruleml-subclass-rel 'pprint-ruleml-subclass-rel)

(defun pprint-ruleml-equal (stream equal)
  (write (ruleml-equal-left equal) :stream stream)
  (write-char #\= stream)
  (write (ruleml-equal-right equal) :stream stream))

(set-pprint-dispatch 'ruleml-equal 'pprint-ruleml-equal)

(defun pprint-ruleml-atom (stream atom)
  (when *inside-psoa-rest*
    (write-char #\# stream))
  (write (ruleml-atom-root atom) :stream stream)
  (when-it (ruleml-atom-descriptors atom)
    (let ((*inside-psoa-rest* t))
      (pprint-fill stream it)))
  (pprint-newline :fill stream))

(set-pprint-dispatch 'ruleml-atom 'pprint-ruleml-atom)

(defun pprint-ruleml-oidful-atom (stream atom)
  (write (ruleml-oidful-atom-oid atom) :stream stream)
  (when (not *inside-psoa-rest*)
    (write-char #\# stream))
  (write (ruleml-oidful-atom-predicate atom) :stream stream))

(set-pprint-dispatch 'ruleml-oidful-atom 'pprint-ruleml-oidful-atom)

(defun pprint-ruleml-forall (stream clause)
  (let* ((forall-prefix (format nil "Forall ~{~a~^ ~} (" (ruleml-forall-vars clause)))
         (prefix-length (length forall-prefix)))
    (pprint-logical-block (stream nil :prefix forall-prefix :suffix ")")
      (pprint-indent :block (+ (- prefix-length) 4) stream)
      (pprint-newline :mandatory stream)
      (write (ruleml-forall-clause clause) :stream stream)
      (pprint-indent :block (- prefix-length) stream)
      (pprint-newline :mandatory stream))))

(set-pprint-dispatch 'ruleml-forall 'pprint-ruleml-forall)

(defun pprint-ruleml-implies (stream implies)
  (write (ruleml-implies-conclusion implies) :stream stream)
  (write-char #\Space stream)
  (write-string ":-" stream)
  (write-char #\Space stream)
  (write (ruleml-implies-condition implies) :stream stream))

(set-pprint-dispatch 'ruleml-implies 'pprint-ruleml-implies)

(defun pprint-ruleml-and (stream item)
  (pprint-logical-block (stream nil :prefix "And(" :suffix ")")
    (pprint-indent :block -3 stream)
    (pprint-newline :mandatory stream)
    (pprint-linear stream (ruleml-and-terms item) nil)
    (pprint-indent :block (- (length "And(")) stream)
    (pprint-newline :mandatory stream)))

(set-pprint-dispatch 'ruleml-and 'pprint-ruleml-and)

(defun pprint-ruleml-or (stream item)
  (pprint-logical-block (stream nil :prefix "Or(" :suffix ")")
    (pprint-indent :block -2 stream)
    (pprint-newline :mandatory stream)
    (pprint-linear stream (ruleml-or-terms item) nil)
    (pprint-indent :block (- (length "Or(")) stream)
    (pprint-newline :mandatory stream)))

(set-pprint-dispatch 'ruleml-or 'pprint-ruleml-or)

(defun pprint-ruleml-exists (stream clause)
  (let* ((exists-prefix (format nil "Exists ~{~a~^ ~} (" (ruleml-exists-vars clause)))
         (prefix-length (length exists-prefix)))
   (pprint-logical-block (stream nil :prefix exists-prefix :suffix ")")
     (pprint-indent :block (+ (- prefix-length) 4) stream)
     (pprint-newline :mandatory stream)
     (write (ruleml-exists-formula clause) :stream stream)
     (pprint-indent :block (- prefix-length) stream)
     (pprint-newline :mandatory stream))))

(set-pprint-dispatch 'ruleml-exists 'pprint-ruleml-exists)

(defun pprint-ruleml-external (stream external)
  (pprint-logical-block (stream nil :prefix "External(" :suffix ")")
    (pprint-indent :block 4 stream)
    (write (ruleml-external-atom external) :stream stream)))

(set-pprint-dispatch 'ruleml-external 'pprint-ruleml-external)

(defun pprint-ruleml-membership (stream membership)
  (write (ruleml-membership-oid membership) :stream stream)
  (write-char #\# stream)
  (write (ruleml-membership-predicate membership) :stream stream))

(set-pprint-dispatch 'ruleml-membership 'pprint-ruleml-membership)

(defun pprint-ruleml-number (stream number)
  (write (ruleml-number-value number) :stream stream))

(set-pprint-dispatch 'ruleml-number 'pprint-ruleml-number)
