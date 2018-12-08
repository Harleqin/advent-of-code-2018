(in-package #:asdf-user)

(defsystem "advent-of-code-2018"
  :author "Svante v. Erichsen <svante.v.erichsen@web.de>"
  :license "public domain/CC0"
  :serial t
  :depends-on ("alexandria" "arrows" "cl-ppcre" "drakma" "local-time")
  :components ((:file "base")
               (:file "1")
               (:file "2")
               (:file "3")
               (:file "4")
               (:file "5")
               (:file "6")
               (:file "7")
               (:file "8")))
