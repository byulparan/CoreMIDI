(in-package :midi)

(defun now ()
  (* 1.0d-9 #+ccl(ccl::current-time-in-nanoseconds)
	    #-ccl(cffi:foreign-funcall "mach_absolute_time" :int64)))


(defconstant +k-cf-string-encoding-utf-8+ #x08000100)

(defmacro with-cf-strings (bindings &body body)
  `(let ,(mapcar (lambda (bind) (list (car bind) nil)) bindings)
     (unwind-protect
	  (progn
	    ,@(loop :for form :in bindings
		    :collect
		    `(setf ,(car form)
			   (cffi:foreign-funcall "CFStringCreateWithCString"
			     :pointer
			     (cffi:foreign-funcall "CFAllocatorGetDefault"
			       :pointer)
			     :string ,(second form)
			     :int +k-cf-string-encoding-utf-8+
			     :pointer)))
	    ,@body)
       ,@(loop :for form :in bindings
	       :collect `(cffi:foreign-funcall "CFRelease"
			   :pointer ,(car form))))))
