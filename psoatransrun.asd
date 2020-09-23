(asdf:defsystem #:psoatransrun
  :description "Describe PSOATransRun here"
  :author "Mark Thom"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:esrap #:rutils #:trivia)
  :components ((:file "package")
               (:file "psoa-ast")
               (:file "psoa-pprint")
               (:file "psoa-grammar")
               (:file "psoa-transformers")))
