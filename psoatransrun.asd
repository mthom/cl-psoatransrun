(asdf:defsystem #:psoatransrun
  :description "Describe PSOATransRun here"
  :author "Mark Thom"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:esrap #:rutils #:trivia)
  :components ((:file "package")
               (:file "psoa-ast")
               (:file "psoa-grammar")
               (:file "psoa-pprint")
               (:file "psoa-prolog-translator")
               (:file "psoa-transformers")
               (:file "psoatransrun")))
