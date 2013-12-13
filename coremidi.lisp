(in-package :midi)

(defun all-endpoints (direction)
  (multiple-value-bind (number-f get-f)
      (ecase direction
	(:input (values #'number-of-sources #'get-source))
	(:output (values #'number-of-destinations #'get-destination)))
    (loop for i from 0 below (funcall number-f)
	  collect (funcall get-f i))))

(defun list-of-sources ()
  "print the name of sources in system."
  (let ((all-sources (all-endpoints :input)))
    (loop for src in all-sources
	  for i from 0
	  do (format t "~2d:  ~a~%" i (midiobject-name src)))))

(defun list-of-destinations ()
  "print the name of destinations in system."
  (let ((all-dest (all-endpoints :output)))
    (loop for dst in all-dest
	  for i from 0
	  do (format t "~2d:  ~a~%" i (midiobject-name dst)))))

(defun get-source (index)
  "Returns one of the sources in the system."
  (let ((src (midigetsource index)))
    (assert (not (zerop src)) (index) "out of index! index: ~a max-index: ~a" index (1- (number-of-sources)))
    src))

(defun get-destination (index)
  "Returns one of the destinations in the system."
  (let ((dst (midigetdestination index)))
    (assert (not (zerop dst)) (index) "out of index! index: ~a max-index: ~a" index (1- (number-of-destinations)))
    dst))


(defun find-source (name)
  "Returns a source which has a same name."
  (find name (all-endpoints :input) :test #'string= :key #'midiobject-name))

(defun find-destination (name)
  "Returns a destination which has a same name."
  (find name (all-endpoints :output) :test #'string= :key #'midiobject-name))


(defvar *midi-client* nil
  "The property-list which have information of current midi-client object.
 That properties are (:client :in-port :out-port :connected-sources :in-action-handlers :virtual-endpoints)")

(defun process-pkt (pkt source)
  "Process to incoming MIDI message via input-port. It called by midi-read-proc which you supplied when create input-port."
  (labels ((get-handle (source status)
	     (let* ((handle-plist (cdr (assoc source (getf *midi-client* :in-action-handlers)))))
	       (getf handle-plist status))))
    (let ((i 0))
      (cffi:with-foreign-slots ((length data) pkt (:struct +midi-packet+))
	(handler-case
	    (loop while (> length i) do
	      (let* ((head (cffi:mem-aref data :unsigned-char i))
		     (status (logand head #xf0))
		     (chan (1+ (logand head #x0f))))
		(case status
		  (#xA0 (incf i 3))	 ;polytouch
		  ((#xC0 #xD0) (incf i 2)) ;program / touch
		  (#xE0 (progn (alexandria:when-let ((handle (get-handle source :bend))) ;bend
				 (funcall handle chan (logior (ash (cffi:mem-aref data :unsigned-char (+ i 2)) 7)
							      (cffi:mem-aref data :unsigned-char (+ i 1)))
					  0))
			       (incf i 3)))
		  (otherwise (progn (alexandria:when-let ((handle (get-handle source (ecase status
										       (#x80 :note-off)
										       (#x90 :note-on)
										       (#xB0 :cc)))))
				      (funcall handle chan (cffi:mem-aref data :unsigned-char (+ i 1))
					       (cffi:mem-aref data :unsigned-char (+ i 2))))
				    (incf i 3))))))
	  (error (c) (format t "error ~a in coremidi handle thread~%" c)))))))


;;; ccl support foreign-thread-callback. so you can provide this callback function to system, directly.
;;; in other implementations(SBCL,ECL) need wrapper function which implemented in C.
#+ccl
(cffi:defcallback midi-read-proc :void ((packet-list :pointer)
					(read-proc-ref-con :pointer)
					(src-conn-ref-con :pointer))
  (declare (ignore read-proc-ref-con))
  (let ((num-packet (cffi:foreign-slot-value packet-list '(:struct +midi-packet-list+) 'num-packets))
	(pkt (cffi:foreign-slot-pointer packet-list '(:struct +midi-packet-list+) 'packet)))
    (loop for i from 0 below num-packet do
      (process-pkt pkt (cffi-sys:pointer-address src-conn-ref-con))
      (setf pkt
	    (cffi-sys:inc-pointer (cffi:foreign-slot-pointer pkt '(:struct +midi-packet+) 'data)
				  (cffi:foreign-slot-value pkt '(:struct +midi-packet+) 'length))))))

(defun set-midi-callback (source status handle)
  "Register handle-function that process incoming MIDI message via input-port."
  (let ((action-handlers (getf *midi-client* :in-action-handlers)))
    (let ((handle-plist (assoc source action-handlers)))
      (unless handle-plist
	(connect-source (getf *midi-client* :in-port) source (cffi-sys:make-pointer source))
	(pushnew source (getf *midi-client* :connected-sources))
	(let ((initial-plist (cons source (list :note-on nil :note-off nil :cc nil :bend nil))))
	  (setf (getf *midi-client* :in-action-handlers) (cons initial-plist action-handlers)
		handle-plist initial-plist)))
      (setf (getf (cdr handle-plist) status) handle))))

 
(defun send-midi-message (destination timestamp status data1 data2)
  (cffi:with-foreign-objects ((pkt-buffer :char 1024)
			      (data :unsigned-char 3))
    (loop for byte in (list status data1 data2)
	  for i from 0
	  do (setf (cffi:mem-aref data :unsigned-char i) byte))
    (let ((pkt (packet-list-init pkt-buffer)))
      (packet-list-add pkt-buffer 1024 pkt timestamp 3 data)
      (midisend (getf *midi-client* :out-port) destination pkt-buffer))))

(defgeneric midi-send (destination timestamp status channel data1 data2)
  (:documentation "Send MIDI message to given destination. timestamp == 0 mean it \"now\".
 Otherwise timestamp must based on time of scheduler."))

(defmethod midi-send (destination (timestamp (eql 0)) status channel data1 data2)
  (send-midi-message destination 0 (+ (1- (alexandria:clamp channel 1 16))
				      (ecase status
					(:note-on #x90)
					(:note-off #x80)
					(:cc #xB0)))
		     data1 data2))

(defmethod midi-send (destination (timestamp float) status channel data1 data2)
  (send-midi-message destination (floor (* 1000000000 timestamp))
		     (+ (1- (alexandria:clamp channel 1 16))
			(ecase status
			  (:note-on #x90)
			  (:note-off #x80)
			  (:cc #xB0)))
		     data1 data2))

(defun dispose-resources-of-client (client)
  "Disposes resources of given client."
  (let ((in-port (getf client :in-port)))
    (dolist (src (getf client :connected-sources))
      (disconnect-source in-port src))
    (dispose-port in-port))
  (dispose-port (getf client :out-port))
  (dolist (end-pnt (getf client :virtual-endpoints))
    (dispose-endpoint end-pnt))
  (dispose-client (getf client :client)))

  
#-ccl
(let ((threads nil))
  (defun start-handle-thread ()
    (unless threads
      (setf threads
	    (bt:make-thread
	     (lambda ()
	       (let ((index 0)
		     (packets-size (cffi:mem-ref (cffi:foreign-symbol-pointer "PACKETS_SIZE") :int))
		     (mutex (cffi:foreign-symbol-pointer "mutex"))
		     (condition-var (cffi:foreign-symbol-pointer "condition_var")))
		 (cffi:with-foreign-object (packet-ref :pointer)
		   (cffi:foreign-funcall "pthread_mutex_lock" :pointer mutex)
		   (loop
		     (cffi:foreign-funcall "pthread_cond_wait" :pointer condition-var
							       :pointer mutex)
		     (loop until (= index (cffi:mem-ref (cffi:foreign-symbol-pointer "current_index") :int))
			   do (let ((src (cffi:foreign-funcall "get_packet" :pointer packet-ref :int index +midi-object-ref+)))
				(process-pkt (cffi:mem-ref packet-ref :pointer) src)
				(setf index (mod (+ index 1) packets-size))))))))
	     :name "MIDI-Handler Thread.")))))
 
(defun coremidi-start ()
  "Prepare a midi-client and required resources."
  (when *midi-client*
    (dispose-resources-of-client *midi-client*))
  (cffi:with-foreign-objects ((client '+midi-object-ref+)
			      (in-port '+midi-object-ref+)
			      (out-port '+midi-object-ref+))
    (with-cf-strings ((client-name "cl-client")
		      (in-portname "in-port-on-cl-client")
		      (out-portname "out-port-on-cl-client"))
      (create-client client-name (cffi-sys:null-pointer) (cffi-sys:null-pointer) client)
      (let ((client (cffi:mem-ref client '+midi-object-ref+)))
	(create-input-port client in-portname
			   #+ccl(cffi:callback midi-read-proc)
			   #-ccl(cffi:foreign-symbol-pointer "midi_read_proc")
			   (cffi-sys:null-pointer)
			   in-port)
	(create-output-port client out-portname out-port) 
	#-ccl (start-handle-thread)
	(setf *midi-client*
	      (list :client client
		    :in-port (cffi:mem-ref in-port '+midi-object-ref+)
		    :out-port (cffi:mem-ref out-port '+midi-object-ref+)
		    :connected-sources nil
		    :in-action-handlers nil
		    :virtual-endpoints nil))))))


