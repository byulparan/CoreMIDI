(asdf/defsystem:defsystem #:coremidi
  :depends-on (#:cffi #:alexandria #-ccl #:bordeaux-threads)
  :components ((:file "package")
	       (:file "util")
	       (:file "cffi")
	       (:file "coremidi")))
