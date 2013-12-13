(asdf/defsystem:defsystem #:coremidi
  :depends-on (#:scheduler #:cffi #:bordeaux-threads #:alexandria #-ccl #:simple-utils)  
  :components ((:file "package")
	       (:file "cffi")
	       (:file "coremidi")))
