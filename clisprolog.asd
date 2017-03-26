;;;; clisprolog.asd

(asdf:defsystem #:clisprolog
  :description "Describe clisprolog here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "data")
               (:file "operator")
               (:file "lexer")
               (:file "reader")
               (:file "parser")
               (:file "clisprolog"))
  :depends-on (:alexandria :split-sequence :iterate))

