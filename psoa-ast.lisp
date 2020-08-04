
(in-package #:psoa-ast)

(eval-when (:compile-toplevel :load-toplevel)
  (setf *arity-check-by-test-call* nil))

(defstruct ruleml-ast-node
  (position 0 :type (integer 0 *)))

(defstruct (ruleml-document (:include ruleml-ast-node))
  base
  prefixes
  imports
  performatives)

(defstruct (ruleml-base (:include ruleml-ast-node))
  iri-ref)

(defstruct (ruleml-prefix (:include ruleml-ast-node))
  name
  iri-ref)

(defstruct (ruleml-const (:include ruleml-ast-node))
  contents)

(defstruct (ruleml-string (:include ruleml-ast-node))
  contents)

(defstruct (ruleml-import (:include ruleml-ast-node))
  iri-ref
  profile)

(defstruct (ruleml-assert (:include ruleml-ast-node))
  items)

(defstruct (ruleml-query (:include ruleml-ast-node))
  term)

(defstruct (ruleml-naf (:include ruleml-ast-node))
  formula)

(defstruct (ruleml-var (:include ruleml-ast-node))
  name)

(defstruct (ruleml-slot (:include ruleml-ast-node))
  dep
  name
  filler)

(defstruct (ruleml-tuple (:include ruleml-ast-node))
  dep
  terms)

(defstruct (ruleml-pname-ln (:include ruleml-ast-node))
  name
  url)

(defstruct (ruleml-expr (:include ruleml-ast-node))
  root
  terms)

(defstruct (ruleml-subclass-rel (:include ruleml-ast-node))
  sub
  super)

(defstruct (ruleml-equal (:include ruleml-ast-node))
  left
  right)

(defstruct (ruleml-atom (:include ruleml-ast-node))
  root
  descriptors)

(defstruct (ruleml-oidful-atom (:include ruleml-ast-node))
  oid
  predicate)

(defstruct (ruleml-forall (:include ruleml-ast-node))
  vars
  clause)

(defstruct (ruleml-implies (:include ruleml-ast-node))
  conclusion
  condition)

(defstruct (ruleml-and (:include ruleml-ast-node))
  terms)

(defstruct (ruleml-or (:include ruleml-ast-node))
  terms)

(defstruct (ruleml-exists (:include ruleml-ast-node))
  vars
  formula)

(defstruct (ruleml-external (:include ruleml-ast-node))
  atom)

(defstruct (ruleml-membership (:include ruleml-ast-node))
  oid
  predicate)

(defun walk-ast (term key &optional (propagator (lambda (term) (walk-ast term key))))
  (match term
    ((ruleml-document :base base :prefixes prefixes
                      :imports imports :performatives performatives)
     (funcall propagator base)
     (mapc propagator prefixes)
     (mapc propagator imports)
     (mapc propagator performatives)
     (funcall key term))
    ((or (ruleml-assert :items terms)
         (ruleml-tuple :terms terms)
         (ruleml-and :terms terms)
         (ruleml-or :terms terms))
     (mapc propagator terms)
     (funcall key term))
    ((or (ruleml-base)
         (ruleml-prefix)
         (ruleml-import)
         (ruleml-pname-ln)
         (ruleml-const))
     (funcall key term))
    ((ruleml-query :term query)
     (funcall propagator query)
     (funcall key term))
    ((ruleml-naf :formula naf)
     (funcall propagator naf)
     (funcall key term))
    ((ruleml-var :name name)
     (funcall propagator name)
     (funcall key term))
    ((ruleml-slot :name name :filler filler)
     (funcall propagator name)
     (funcall propagator filler)
     (funcall key term))
    ((ruleml-expr :root root :terms terms)
     (funcall propagator root)
     (mapc propagator terms)
     (funcall key term))
    ((ruleml-subclass-rel :sub sub :super super)
     (funcall propagator sub)
     (funcall propagator super)
     (funcall key term))
    ((ruleml-equal :left left :right right)
     (funcall propagator left)
     (funcall propagator right)
     (funcall key term))
    ((ruleml-implies :conclusion conclusion :condition condition)
     (funcall propagator conclusion)
     (funcall propagator condition)
     (funcall key term))
    ((ruleml-exists :vars vars :formula formula)
     (mapc propagator vars)
     (funcall propagator formula)
     (funcall key term))
    ((ruleml-atom :root root :descriptors descriptors)
     (funcall propagator root)
     (mapc propagator descriptors)
     (funcall key term))
    ((ruleml-oidful-atom :oid oid :predicate atom)
     (funcall propagator oid)
     (funcall propagator atom)
     (funcall key term))
    ((ruleml-forall :vars vars :clause clause)
     (mapc propagator vars)
     (funcall propagator clause)
     (funcall key term))
    ((ruleml-external :atom atom)
     (funcall propagator atom)
     (funcall key term))
    ((ruleml-membership :predicate predicate)
     (funcall propagator predicate)
     (funcall key term))))
