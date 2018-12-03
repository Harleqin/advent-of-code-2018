(in-package #:asdf-user)

(defsystem "advent-of-code-2018"
  :author "Svante v. Erichsen <svante.v.erichsen@web.de>"
  :license "public domain/CC0"
  :serial t
  :depends-on ("alexandria" "arrows" "cl-ppcre" "drakma")
  :components ((:file "package")
               (:file "util")
               (:file "1")
               (:file "2")
               (:file "3")))
