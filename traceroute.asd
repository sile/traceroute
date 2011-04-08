(require :asdf)

(defsystem traceroute
  :name "traceroute"
  :author "Takeru Ohta"
  :version "0.0.1"
  :description "traceroute"

  :depends-on (:sb-bsd-sockets)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "alien-util")
               (:file "type")
               (:file "traceroute")))
