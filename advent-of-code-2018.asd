(in-package #:asdf-user)

(defsystem "advent-of-code-2018"
  :author "Svante v. Erichsen <svante.v.erichsen@web.de>"
  :license "public domain/CC0"
  :serial t
  :depends-on ("alexandria"
               "arrows"
               "cl-ppcre"
               "drakma"
               "for"
               "let-plus"
               "local-time"
               "minheap"
               "opticl"
               "split-sequence")
  :components ((:file "base")
               (:file "cookie-jar")
               (:file "1")
               (:file "2")
               (:file "3")
               (:file "4")
               (:file "5")
               (:file "6")
               (:file "7")
               (:file "8")
               (:file "9")
               (:file "10")
               (:file "11")
               (:file "12")
               (:file "13")
               (:file "14")
               (:file "15")
               (:file "16")
               (:file "17")))
