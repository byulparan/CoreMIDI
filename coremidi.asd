(asdf/defsystem:defsystem #:coremidi
  :depends-on (#:scheduler #:cffi  #:alexandria #-ccl #:bordeaux-threads #-ccl #:simple-utils)  
  :components ((:file "package")
	       (:file "cffi")
	       (:file "coremidi")))
