(in-package #:asdf-user)

(defsystem "advent-of-code-2018"
  :author "Svante v. Erichsen <svante.v.erichsen@web.de>"
  :license "public domain/CC0"
  :serial t
  :depends-on ("alexandria" "arrows")
  :components ((:file "package")
               (:file "1")))
