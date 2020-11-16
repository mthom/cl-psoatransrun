(asdf:defsystem #:psoatransrun
  :description "A CL implementation of PSOATransRun, realizing the PSOA RuleML data and rule language."
  :author "Mark Thom"
  :license "BSD-3"
  :version "0.9"
  :serial t
  :depends-on (#:drakma #:esrap #:pathname-utils #:rutils #:trivia #:usocket)
  :components ((:file "package")
               (:file "prolog-engine-client")
               (:file "prolog-grammar")
               (:file "psoa-ast")
               (:file "psoa-grammar")
               (:file "psoa-pprint")
               (:file "psoa-prolog-translator")
               (:file "psoa-transformers")
               (:file "psoatransrun")
               (:file "psoatransrun-tests")))
