
(in-package #:psoa-ast)

(eval-when (:compile-toplevel :load-toplevel)
  (setf *arity-check-by-test-call* nil))

(named-readtables:in-readtable rutils-readtable)

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

(defstruct (ruleml-genvar (:include ruleml-var)))

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

(defun transform-ast (term key &key positive negative external
                                 (propagator
                                  (lambda (term &key (positive positive) (negative negative)
                                                  (external external))
                                    (transform-ast term key
                                                   :positive positive :negative negative
                                                   :external external))))
  (match term
    ((ruleml-document :base base :prefixes prefixes
                      :imports imports :performatives performatives)
     (funcall key
              (make-ruleml-document
               :base (funcall propagator base)
               :prefixes (mapcar propagator prefixes)
               :imports (mapcar propagator imports)
               :performatives (mapcar propagator performatives))))
    ((ruleml-assert :items terms)
     (funcall key (make-ruleml-assert :items (mapcar propagator terms))
              :positive positive
              :negative negative))
    ((ruleml-tuple :terms terms :dep dep)
     (funcall key (make-ruleml-tuple :terms (mapcar propagator terms) :dep dep)
              :positive positive
              :negative negative
              :external external))
    ((ruleml-and :terms terms)
     (funcall key (make-ruleml-and :terms (mapcar propagator terms))
              :positive positive
              :negative negative
              :external external))
    ((ruleml-or :terms terms)
     (funcall key (make-ruleml-or :terms (mapcar propagator terms))
              :positive positive
              :negative negative
              :external external))
    ((or (ruleml-base)
         (ruleml-prefix)
         (ruleml-import)
         (ruleml-pname-ln)
         (ruleml-const)
         (ruleml-var))
     (funcall key term
              :positive positive
              :negative negative
              :external external))
    ((ruleml-query :term query)
     (funcall key (make-ruleml-query :term (funcall propagator query :positive t :negative t))))
    ((ruleml-naf :formula naf)
     (funcall key (make-ruleml-naf :formula (funcall propagator naf))
              :positive positive
              :negative negative
              :external external))
    ((ruleml-slot  :name name :filler filler :dep dep)
     (funcall key (make-ruleml-slot :dep dep
                                    :name (funcall propagator name)
                                    :filler (funcall propagator filler))
              :positive positive
              :negative negative
              :external external))
    ((ruleml-expr :root root :terms terms)
     (funcall key (make-ruleml-expr :root (funcall propagator root)
                                    :terms (mapcar propagator terms))
              :positive positive
              :negative negative
              :external external))
    ((ruleml-subclass-rel :sub sub :super super)
     (funcall key (make-ruleml-subclass-rel :sub (funcall propagator sub)
                                            :super (funcall propagator super))
              :positive positive
              :negative negative
              :external external))
    ((ruleml-equal :left left :right right)
     (funcall key (make-ruleml-equal :left  (funcall propagator left)
                                     :right (funcall propagator right))
              :positive positive
              :negative negative
              :external external))
    ((ruleml-implies :conclusion conclusion :condition condition)
     (funcall key (make-ruleml-implies
                   :conclusion (funcall propagator conclusion
                                        :positive t :negative nil)
                   :condition (funcall propagator condition
                                       :positive nil :negative t))
              :positive positive
              :negative negative))
    ((ruleml-exists :vars vars :formula formula)
     (funcall key (make-ruleml-exists :vars (mapcar propagator vars)
                                      :formula (funcall propagator formula))
              :positive positive
              :negative negative
              :external external))
    ((ruleml-atom :root root :descriptors descriptors)
     (funcall key (make-ruleml-atom :root (funcall propagator root)
                                    :descriptors (mapcar propagator descriptors))
              :positive positive
              :negative negative
              :external external))
    ((ruleml-oidful-atom :oid oid :predicate atom)
     (funcall key (make-ruleml-oidful-atom :oid (funcall propagator oid)
                                           :predicate (funcall propagator atom))
              :positive positive
              :negative negative
              :external external))
    ((ruleml-forall :vars vars :clause clause)
     (funcall key (make-ruleml-forall :vars (mapcar propagator vars)
                                      :clause (funcall propagator clause))
              :positive positive
              :negative negative
              :external external))
    ((ruleml-external :atom atom)
     (funcall key (make-ruleml-external :atom (funcall propagator atom :external t))
              :positive positive
              :negative negative
              :external external))
    ((ruleml-membership :oid oid :predicate predicate)
     (funcall key (make-ruleml-membership :oid (funcall propagator oid)
                                          :predicate (funcall propagator predicate))
              :positive positive
              :negative negative
              :external external))
    (_ (funcall key term
                :positive positive
                :negative negative
                :external external))))
