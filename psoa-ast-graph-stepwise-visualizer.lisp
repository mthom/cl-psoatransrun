
(in-package #:psoa-ast-graph-stepwise-visualizer)

;; EXAMPLE: draw a series of graphs illustrating the stepwise
;; application of (transform-ast κ ρ) to a ruleml-atom. Or, apply the
;; default transform-ast recursion stepwise to a sequence of atoms.

(defstruct tree-step-function
  term)

(defstruct (kappa-wrapped (:include tree-step-function)))

(defstruct (rho-wrapped (:include tree-step-function)))

(defun pprint-kappa-wrapped (stream kappa-wrapped)
  (write "κ(" :stream stream :escape nil)
  (write (kappa-wrapped-term kappa-wrapped) :stream stream :escape nil)
  (write ")" :stream stream :escape nil))

(set-pprint-dispatch 'kappa-wrapped 'pprint-kappa-wrapped)

(defun pprint-rho-wrapped (stream rho-wrapped)
  (write "ρ(" :stream stream :escape nil)
  (write (rho-wrapped-term rho-wrapped) :stream stream :escape nil)
  (write ")" :stream stream :escape nil))

(set-pprint-dispatch 'rho-wrapped 'pprint-rho-wrapped)

(defmethod cl-dot:graph-object-node ((graph (eql 'psoa-ast-graph::ruleml-ast-graph)) (object tree-step-function))
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(format nil "~A" object)
                               :shape :box)))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'psoa-ast-graph::ruleml-ast-graph)) (object tree-step-function))
  (list (make-instance 'cl-dot:attributed
                       :object (tree-step-function-term object)
                       :attributes '(:weight 3))))

(defun stepwise-transform-ast-application (ruleml-atom)
  "This function produces a \"free\" representation of a single transformation step of the argument
\"ruleml-atom\" by the evaluation of

* (transform-ast ruleml-atom κ ρ)

where κ and ρ are the key and propagator functions documented in psoa-ast.lisp.

ρ is necessarily the fixed-point default of transform-ast occurring as
the final, implicit argument of the form (transform-ast κ).

The actions of κ and ρ are represented in the return value term by
wrapping the argument x of either function in the free terms \"κ(x)̈\"
and \"ρ(x)\". These terms are pretty-printed within the transformed
tree as they occur.

An example REPL interaction:

* (esrap:parse 'psoa-grammar::atom \"o(_ _a _b)#p(+[_c _])\")
o(_ _a _b)#p(+[_c _])
NIL
T
* (stepwise-transform-ast-application *)
κ(ρ(o(_ _a _b))#ρ(p(_c _)))
T
* (stepwise-transform-ast-application *)
κ(κ(ρ(o)(ρ(_) ρ(_a) ρ(_b)))#κ(ρ(p)(ρ(+[_c _]))))
T
* (stepwise-transform-ast-application *)
κ(κ(κ(o)(κ(_) κ(_a) κ(_b)))#κ(κ(p)(κ(+[ρ(_c) ρ(_)]))))
T
* (stepwise-transform-ast-application *)
κ(κ(κ(o)(κ(_) κ(_a) κ(_b)))#κ(κ(p)(κ(+[κ(_c) κ(_)]))))
T
* (stepwise-transform-ast-application *)
κ(κ(κ(o)(κ(_) κ(_a) κ(_b)))#κ(κ(p)(κ(+[κ(_c) κ(_)]))))
NIL

The second return value, a boolean, is T iff the output atom (the
first return value) differs from the input \"ruleml-atom\"."
  (let (rho-expanded-p)
    (flet ((make-kappa-wrapped (&key term)
             ;; Use flet to shadow the default make-kappa-wrapped and
             ;; also to allow the use of the default inside this
             ;; make-kappa-wrapped.
             (if (kappa-wrapped-p term)
                 term
                 (make-kappa-wrapped :term term))))
      (labels ((key (term &key &allow-other-keys)
                 (match term
                   ((kappa-wrapped :term term)
                    (make-kappa-wrapped :term (transform-ast term #'key :propagator #'propagator)))
                   ((rho-wrapped)
                    term)
                   (_
                    (make-kappa-wrapped :term term))))
               (propagator (term &key &allow-other-keys)
                 (match term
                   ((rho-wrapped :term term)
                    (setf rho-expanded-p t)
                    (make-kappa-wrapped :term (transform-ast term #'key :propagator #'propagator)))
                   ((kappa-wrapped)
                    (key term))
                   (_
                    (setf rho-expanded-p t)
                    (make-rho-wrapped :term term)))))
        (values (transform-ast ruleml-atom #'key :propagator #'propagator)
                rho-expanded-p)))))

(defun evaluate-κ-in-transform-ast-application (ruleml-atom κ)
  (labels ((key (term &key &allow-other-keys)
             (match term
               ((kappa-wrapped :term term)
                (funcall κ (transform-ast term #'key :propagator #'propagator)))
               (_
                term)))
           (propagator (term &key &allow-other-keys)
             (match term
               ((kappa-wrapped) (key term))
               (_ (transform-ast term #'key :propagator #'propagator)))))
    (transform-ast ruleml-atom #'key :propagator #'propagator)))

(defun visualize-transform-ast-application (term filename-prefix &optional (κ #'identity))
  (flet ((generate-filename (filename-counter)
           (format nil "~A~D.png" filename-prefix filename-counter)))
    (loop with rho-expanded-p = nil
          for file-counter upfrom 1
          for filename = (generate-filename file-counter)
          do (generate-psoa-ast-graph-from-atom term filename)
             (multiple-value-setq (term rho-expanded-p)
               (stepwise-transform-ast-application term))
          collect filename into graph-filenames
          unless rho-expanded-p
            do (let ((filename #1=(generate-filename (1+ file-counter))))
                 (generate-psoa-ast-graph-from-atom
                  (evaluate-κ-in-transform-ast-application term κ)
                  filename))
            and collect #1# into graph-filenames
            and return graph-filenames)))
