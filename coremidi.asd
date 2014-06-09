(asdf/defsystem:defsystem #:coremidi
  :depends-on (#:scheduler #:cffi  #:alexandria #-ccl #:bordeaux-threads #-ccl #:simple-utils)
  :version "0.1.0"
  :components ((:file "package")
	       (:file "cffi")
	       (:file "coremidi")))
