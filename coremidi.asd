(asdf/defsystem:defsystem #:coremidi
  :depends-on (#:cffi #:alexandria #-(or sbcl ccl) #:bordeaux-threads)
  :version "2017.3.14"
  :components ((:file "package")
	       (:file "util")
	       (:file "cffi")
	       (:file "coremidi")))
