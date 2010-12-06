;;;; sassetti.asd

(asdf:defsystem #:sassetti
  :serial t
  :depends-on (#:vecto)
  :components ((:file "package")
               (:file "sassetti")))

