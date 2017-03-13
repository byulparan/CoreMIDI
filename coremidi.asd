(asdf/defsystem:defsystem #:coremidi
  :depends-on (#:cffi #:alexandria #-ccl #:bordeaux-threads)
  :version "2017.3.14"
  :components ((:file "package")
	       (:file "cffi")
	       (:file "coremidi")))
