(asdf:defsystem #:configdb
  :depends-on ("hunchentoot"
	       "alexandria")
  :components ((:file "package")
	       (:file "range-generator")))
