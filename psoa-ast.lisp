
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

(defun transform-ast (term key &optional (propagator (lambda (term) (transform-ast term key))))
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
     (funcall key (make-ruleml-assert :items (mapcar propagator terms))))
    ((ruleml-tuple :terms terms :dep dep)
     (funcall key (make-ruleml-tuple :terms (mapcar propagator terms) :dep dep)))
    ((ruleml-and :terms terms)
     (funcall key (make-ruleml-and :terms (mapcar propagator terms))))
    ((ruleml-or :terms terms)
     (funcall key (make-ruleml-or :terms (mapcar propagator terms))))
    ((or (ruleml-base)
         (ruleml-prefix)
         (ruleml-import)
         (ruleml-pname-ln)
         (ruleml-const)
         (ruleml-var))
     (funcall key term))
    ((ruleml-query :term query)
     (funcall key (make-ruleml-query :term (funcall propagator query))))
    ((ruleml-naf :formula naf)
     (funcall key (make-ruleml-naf :formula (funcall propagator naf))))
    ((ruleml-slot :name name :filler filler :dep dep)
     (funcall key (make-ruleml-slot :dep dep
                                    :name (funcall propagator name)
                                    :filler (funcall propagator filler))))
    ((ruleml-expr :root root :terms terms)
     (funcall key (make-ruleml-expr :root (funcall propagator root)
                                    :terms (mapcar propagator terms))))
    ((ruleml-subclass-rel :sub sub :super super)
     (funcall key (make-ruleml-subclass-rel :sub (funcall propagator sub)
                                            :super (funcall propagator super))))
    ((ruleml-equal :left left :right right)
     (funcall key (make-ruleml-equal :left  (funcall propagator left)
                                     :right (funcall propagator right))))
    ((ruleml-implies :conclusion conclusion :condition condition)
     (funcall key (make-ruleml-implies :conclusion (funcall propagator conclusion)
                                       :condition (funcall propagator condition))))
    ((ruleml-exists :vars vars :formula formula)
     (funcall key (make-ruleml-exists :vars (mapcar propagator vars)
                                      :formula (funcall propagator formula))))
    ((ruleml-atom :root root :descriptors descriptors)
     (funcall key (make-ruleml-atom :root (funcall propagator root)
                                    :descriptors (mapcar propagator descriptors))))
    ((ruleml-oidful-atom :oid oid :predicate atom)
     (funcall key (make-ruleml-oidful-atom :oid (funcall propagator oid)
                                           :predicate (funcall propagator atom))))
    ((ruleml-forall :vars vars :clause clause)
     (funcall key (make-ruleml-forall :vars (mapcar propagator vars)
                                      :clause (funcall propagator clause))))
    ((ruleml-external :atom atom)
     (funcall key (make-ruleml-external :atom (funcall propagator atom))))
    ((ruleml-membership :oid oid :predicate predicate)
     (funcall key (make-ruleml-membership :oid (funcall propagator oid)
                                          :predicate (funcall propagator predicate))))
    (_ (funcall key term))))
