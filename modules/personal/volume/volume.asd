;;;; volume.asd

(asdf:defsystem #:volume
  :serial t
  :description "Shows a bar for current volume"
  :author "Em Segerbäck"
  :depends-on (#:trivial-timeout)
  :components ((:file "package")
               (:file "volume")))

