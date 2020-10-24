
(in-package #:psoatransrun)

(defun psoa-document->prolog (document)
  (-> (parse 'psoa-grammar::ruleml document)
      transform-document
      translate-document))

(defun psoa-query->prolog (query &optional (relationships (make-hash-table :test #'equalp)))
  (-> (parse 'psoa-grammar::query query)
      (transform-query relationships)
      translate-query))

(defun psoa-repl (&optional (relationships (make-hash-table :test #'equalp)))
  (loop for line = (progn (write-string "> ")
                          (read-line *standard-input* nil))
        while line do (write (psoa-query->prolog line relationships))))

(defun psoa-load-and-repl (document)
  (with ((prolog-kb-string relationships (psoa-document->prolog document)))
    (format t "The translated KB:~%~A" prolog-kb-string)
    (psoa-repl relationships)))
