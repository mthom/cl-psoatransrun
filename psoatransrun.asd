(asdf:defsystem #:psoatransrun
  :description "A CL implementation of PSOATransRun, realizing the PSOA RuleML data and rule language."
  :author "Mark Thom"
  :license "BSD-3"
  :version "1.0"
  :serial t
  :depends-on (#:cl-dot #:drakma #:esrap #:pathname-utils #:rutils
               #:trivia #:uiop #:usocket)
  :components ((:file "package")
               (:file "prolog-engine-client")
               (:file "prolog-grammar")
               (:file "psoa-ast")
               (:file "psoa-ast-graph")
               (:file "psoa-ast-graph-visualizer")
               (:file "psoa-grammar")
               (:file "psoa-pprint")
               (:file "psoa-prolog-translator")
               (:file "psoa-transformers")
               (:file "psoatransrun")
               (:file "psoatransrun-tests")))
