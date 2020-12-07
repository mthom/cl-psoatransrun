
(in-package #:psoa-ast-graph)

;; EXPORTS

(defun generate-psoa-ast-graph-from-atom (ruleml-atom graph-filename)
  (assert (ends-with ".png" graph-filename))

  (let ((dgraph (cl-dot:generate-graph-from-roots 'ruleml-ast-graph (list ruleml-atom))))
    (cl-dot:dot-graph dgraph graph-filename :format :png)))


;; RULEML-AST-NODE

(defmethod cl-dot:graph-object-node ((graph (eql 'ruleml-ast-graph)) (object ruleml-ast-node))
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(format nil "~A" object)
                               :shape :box)))

;; RULEML-OIDFUL-ATOM

(defmethod cl-dot:graph-object-points-to ((graph (eql 'ruleml-ast-graph)) (object ruleml-oidful-atom))
  (nreverse (list (make-instance 'cl-dot:attributed
                                 :object (ruleml-oidful-atom-oid object)
                                 :attributes '(:weight 3))
                  (make-instance 'cl-dot:attributed
                                 :object (ruleml-oidful-atom-predicate object)
                                 :attributes '(:weight 3)))))

;; RULEML-ATOM

(defmethod cl-dot:graph-object-points-to ((graph (eql 'ruleml-ast-graph)) (object ruleml-atom))
  (nreverse (list* (make-instance 'cl-dot:attributed
                                  :object (ruleml-atom-root object)
                                  :attributes '(:weight 3))
                   (mapcar (lambda (term)
                             (make-instance 'cl-dot:attributed
                                            :object term
                                            :attributes '(:weight 3)))
                           (ruleml-atom-descriptors object)))))


;; RULEML-TUPLE

(defmethod cl-dot:graph-object-points-to ((graph (eql 'ruleml-ast-graph)) (object ruleml-tuple))
  (nreverse (mapcar (lambda (term)
                      (make-instance 'cl-dot:attributed
                                     :object term
                                     :attributes '(:weight 3)))
                    (ruleml-tuple-terms object))))


;; RULEML-EXPR

(defmethod cl-dot:graph-object-points-to ((graph (eql 'ruleml-ast-graph)) (object ruleml-expr))
  (nreverse (list* (make-instance 'cl-dot:attributed
                                  :object (ruleml-expr-root object)
                                  :attributes '(:weight 3))
                   (mapcar (lambda (term)
                             (make-instance 'cl-dot:attributed
                                            :object term
                                            :attributes '(:weight 3)))
                           (ruleml-expr-terms object)))))

