(asdf/defsystem:defsystem #:coremidi
  :depends-on (#:cffi #:alexandria #-(or sbcl ccl) #:bordeaux-threads)
  :components ((:file "package")
	       (:file "util")
	       (:file "cffi")
	       (:file "coremidi")))
