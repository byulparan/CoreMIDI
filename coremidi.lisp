(in-package :midi)

(defun all-endpoints (direction)
  (multiple-value-bind (number-f get-f)
      (ecase direction
	(:input (values #'get-number-of-sources #'get-source))
	(:output (values #'get-number-of-destinations #'get-destination)))
    (loop for i from 0 below (funcall number-f)
	  collect (funcall get-f i))))

(defun list-of-sources ()
  "print the name of sources in system."
  (let ((all-sources (all-endpoints :input)))
    (loop for src in all-sources
	  for i from 0
	  do (format t "~2d:  ~a~%" i (midiobject-display-name src)))))

(defun list-of-destinations ()
  "print the name of destinations in system."
  (let ((all-dest (all-endpoints :output)))
    (loop for dst in all-dest
	  for i from 0
	  do (format t "~2d:  ~a~%" i (midiobject-display-name dst)))))


(defun find-source (name)
  "Returns a source which has a same name."
  (find name (all-endpoints :input)
	:test #'string= :key #'midiobject-display-name))

(defun find-destination (name)
  "Returns a destination which has a same name."
  (find name (all-endpoints :output)
	:test #'string= :key #'midiobject-display-name))


(defvar *midi-client* nil
  "The property-list which have information of current midi-client object.
 That properties are (:client :in-port :out-port :connected-sources :in-action-handlers :virtual-endpoints)")

(defun process-packet (pkt source)
  "Process PKT coming from SOURCE.
This function is called by HANDLE-INCOMING-PACKETS, supplied when creating an
input port."
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
		  (#xE0 (progn
			  (alexandria:when-let ((handle (get-handle source :bend))) ;bend
			    (funcall handle chan (logior (ash (cffi:mem-aref data :unsigned-char (+ i 2)) 7)
							 (cffi:mem-aref data :unsigned-char (+ i 1)))
				     0))
			  (incf i 3)))
		  (otherwise (progn (alexandria:when-let* 
					((value (cffi:mem-aref data :unsigned-char (+ i 2)))
					 (handle (get-handle source (cond ((or (= status #x80) (and (= status #x90) (zerop value))) :note-off)
									  ((= status #x90) :note-on)
									  ((= status #xB0) :cc)))))
				      (funcall handle chan (cffi:mem-aref data :unsigned-char (+ i 1)) value))
				    (incf i 3))))))
	  (error (c) (format t "error ~a in coremidi handle thread~%" c)))))))


;;; CCL has FOREIGN-THREAD-CALLBACK, so we can provide the callback function
;;; below directly to the system. In other implementations, we need a C
;;; wrapper.
#+ccl
(cffi:defcallback handle-incoming-packets :void ((pktlist :pointer)
						 (read-proc-ref-con :pointer)
						 (src-conn-ref-con :pointer))
  (declare (ignore read-proc-ref-con))
  (let ((packets-number (cffi:foreign-slot-value
			 pktlist '(:struct +midi-packet-list+) 'num-packets))
	(packet (cffi:foreign-slot-pointer
		 pktlist '(:struct +midi-packet-list+) 'packet)))
    (loop for i from 0 below packets-number do
      (process-packet packet (cffi-sys:pointer-address src-conn-ref-con))
      (setf packet (cffi-sys:inc-pointer
		    (cffi:foreign-slot-pointer packet '(:struct +midi-packet+)
					       'data)
		    (cffi:foreign-slot-value packet '(:struct +midi-packet+)
					     'length))))))

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


;;; send-midi-message to destination
(defun send-midi-message (destination timestamp status data1 data2)
  (let (data-length bytes)
    (if data2
	(setq data-length 3
	      bytes (list status data1 data2))
      (setq data-length 2
	    bytes (list status data1)))
    (cffi:with-foreign-objects ((pkt-buffer :char 1024)
				(data :unsigned-char data-length))
      (loop for byte in (mapcar #'floor bytes)
	    for i from 0
	    do (setf (cffi:mem-aref data :unsigned-char i) byte))
      (let ((pkt (packet-list-init pkt-buffer)))
	(packet-list-add pkt-buffer 1024 pkt timestamp data-length data)
	(midisend (getf *midi-client* :out-port) destination pkt-buffer)))))

(defun midi-send-at (hosttime destination status channel data1 &optional data2)
  (send-midi-message destination hosttime
		     (+ (1- (alexandria:clamp channel 1 16))
			(ecase status
			  (:note-off #x80)
			  (:note-on #x90)
			  (:cc #xB0)
			  (:program-change #xC0)))
		     data1 data2))

(defun midi-send (destination status channel data1 &optional data2)
  (apply #'midi-send-at
    0 destination status channel data1 (when data2 (list data2))))

;; #### NOTE: is this needed? Doesn't MIDIClientDispose perform this cleanup?
(defun dispose-resources-of-client (client)
  "Disposes resources of given client."
  (let ((in-port (getf client :in-port)))
    (dolist (src (getf client :connected-sources))
      (disconnect-source in-port src))
    (dispose-port in-port))
  (dispose-port (getf client :out-port))
  (dolist (end-pnt (getf client :virtual-endpoints))
    (endpoint-dispose end-pnt))
  (client-dispose (getf client :client)))

#-ccl
(let (thread)
  (defun create-incoming-packets-handler-thread ()
    (unless thread
      (setf thread
	    (bt:make-thread
	     (lambda ()
	       (let ((lock (cffi:foreign-symbol-pointer "incoming_packets"))
		     (ready
		       (cffi:foreign-symbol-pointer "incoming_packet_ready"))
		     (handled
		       (cffi:foreign-symbol-pointer "incoming_packet_handled"))
		     (flag (cffi:foreign-symbol-pointer "incomingPacketFlag"))
		     (packet (cffi:foreign-symbol-pointer "incomingPacket"))
		     (endpoint
		       (cffi:foreign-symbol-pointer "incomingPacketEndpoint")))
		 (cffi:foreign-funcall "pthread_mutex_lock" :pointer lock)
		 (loop
		   (cffi:foreign-funcall "pthread_cond_wait"
		     :pointer ready :pointer lock)
		   (let ((flag-value (cffi:mem-ref flag :int)))
		     (assert (or (zerop flag-value) (= 1 flag-value))
			     nil
			     "Incoming packets handler thread out of sync.")
		     (when (= 1 flag-value)
		       (decf (cffi:mem-ref flag :int))
		       (process-packet
			(cffi:mem-ref packet :pointer)
			(cffi:mem-ref endpoint 'object-ref))))
		   (cffi:foreign-funcall "pthread_cond_signal"
		     :pointer handled))))
	     :name "Incoming packets handler")))))

(defvar *midi-notify-handler* nil)

(defun add-midi-notify-callback (handle)
  (alexandria:appendf *midi-notify-handler* (list handle)))

(defun set-midi-notify-callback (handle)
  (setf *midi-notify-handler* (list handle)))

(defconstant +setup-changed+ 1)
(defconstant +object-added+ 2)
(defconstant +object-removed+ 3)
(defconstant +property-changed+ 4)
(defconstant +thru-connections-changed+ 5)
(defconstant +serial-port-owner-changed+ 6)
(defconstant +io-error+ 7)


(cffi:defcallback midi-notify-proc :void ((message :pointer) (ref-con :pointer))
  (declare (ignorable message ref-con))
  (cffi:with-foreign-slots ((message-id message-size) message (:struct midi-notification))
    (handler-case
	(dolist (h *midi-notify-handler*)
	  (funcall h message-id message-size))
      (error (c) (format t "~a error...while call midi-notify-proc~%" c)))))

(defun initialize ()
  "Prepare a midi-client and required resources."
  (unless *midi-client*
    (cffi:with-foreign-objects ((client 'object-ref)
				(in-port 'object-ref)
				(out-port 'object-ref))
      (with-cf-strings ((client-name "cl-client")
			(in-portname "in-port-on-cl-client")
			(out-portname "out-port-on-cl-client"))
	(client-create client-name
		       (cffi:callback midi-notify-proc)
		       (cffi-sys:null-pointer)
		       client)
	(let ((client (cffi:mem-ref client 'object-ref)))
	  (create-input-port client in-portname
			     #+ccl(cffi:callback handle-incoming-packets)
			     #-ccl(cffi:foreign-symbol-pointer
				   "handleIncomingPackets")
			     (cffi-sys:null-pointer)
			     in-port)
	  (create-output-port client out-portname out-port)
	  #-ccl (create-incoming-packets-handler-thread)
	  (setf *midi-client*
		(list :client client
		      :in-port (cffi:mem-ref in-port 'object-ref)
		      :out-port (cffi:mem-ref out-port 'object-ref)
		      :connected-sources nil
		      :in-action-handlers nil
		      :virtual-endpoints nil)))))))
