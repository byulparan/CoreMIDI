(in-package :midi)

#-ccl
(handler-case
    (let* ((dir (concatenate 'string
		  (namestring (asdf:system-source-directory :coremidi))
		  "ObjectiveC/"))
	   (file (concatenate 'string dir "libwrapper.dylib")))
      (unless (probe-file file)
	(uiop:run-program (concatenate 'string "make -C " dir) :output t))
      (cffi:load-foreign-library file))
  (error ())) ;; #### FIXME: WTF?

#+ccl
(progn
  (cffi:define-foreign-library coremidi (:darwin (:framework "CoreMIDI")))
  (cffi:use-foreign-library coremidi))

(defun now ()
  (* 1.0d-9 #+ccl(ccl::current-time-in-nanoseconds)
	    #-ccl(cffi:foreign-funcall "mach_absolute_time" :int64)))


;; ==========================================================================
;; Data Types
;; ==========================================================================

(cffi:defctype object-ref :unsigned-int
  "The base class of many CoreMIDI objects.")



;;; MIDI Devices
(cffi:defcfun (number-of-devices "MIDIGetNumberOfDevices") :int
  "Returns the number of devices in the system.")

(cffi:defcfun (get-device "MIDIGetDevice") object-ref
  "Returns one of the devices in the system."
  (index :int))

(cffi:defcfun (number-of-entities-in-device "MIDIDeviceGetNumberOfEntities") :int
  "Returns the number of entities in a given device."
  (device object-ref))

(cffi:defcfun (get-entity-in-device "MIDIDeviceGetEntity") object-ref
  "Returns one of a given device's entities."
  (device object-ref)
  (index :int))

;;; MIDI Entities
(cffi:defcfun (get-device-of-entity "MIDIEntityGetDevice") :int
  "Returns an entity's device."
  (entity object-ref)
  (device-ref :pointer))

(cffi:defcfun (number-of-destinations-in-entity "MIDIEntityGetNumberOfDestinations") :int
  "Returns the number of destinations in a given entity."
  (entity object-ref))

(cffi:defcfun (number-of-sources-in-entity "MIDIEntityGetNumberOfSources") :int
  "Returns the number of sources in a given entity."
  (entity object-ref))

(cffi:defcfun (get-source-in-entity "MIDIEntityGetSource") object-ref
  "Returns one of a given entity's sources"
  (entity object-ref)
  (index :int))

(cffi:defcfun (get-destination-in-entity "MIDIEntityGetDestination") object-ref
  "Returns one of a given entity's destinations."
  (entity object-ref)
  (index :int))

;;; MIDI Endpoints
;;; input-Endpoint is "Source" and output-Endpoint is "Destination"

(cffi:defcfun (number-of-destinations "MIDIGetNumberOfDestinations") :int
  "Returns the number of destinations in the system.")

(cffi:defcfun (number-of-sources "MIDIGetNumberOfSources") :int
  "Returns the number of sources in the system.")

(cffi:defcfun "MIDIGetDestination" object-ref
  "Returns one of the destinations in the system."
  (index :int))

(cffi:defcfun "MIDIGetSource" object-ref
  "Returns one of the sources in the system."
  (index :int))

(cffi:defcfun (get-entity-of-endpoint "MIDIEndpointGetEntity") :int
  "Returns an endpoint's entity."
  (endpoint object-ref)
  (entity-ref :pointer))

(cffi:defcfun (create-destination "MIDIDestinationCreate") :int
  "Creates a virtual destination in client."
  (client object-ref)
  (name :pointer)
  (read-proc :pointer)
  (ref-con :pointer)
  (out-dest :pointer))

(cffi:defcfun (create-source "MIDISourceCreate") :int
  "Creates a virtual source in a client."
  (client object-ref)
  (name :pointer)
  (out-src :pointer))

(cffi:defcfun (dispose-endpoint "MIDIEndpointDispose") :int
  "Disposes a virtual source or destination your client created."
  (endpt object-ref))



;;; MIDIClient
(cffi:defcstruct midi-notification
  (message-id :int)
  (message-size :unsigned-int))

(cffi:defcfun (create-client "MIDIClientCreate") :int
  "Creates a MIDIClient object."
  (name :pointer)
  (notify-proc :pointer)
  (notify-ref-con :pointer)
  (client-ref :pointer))

(cffi:defcfun (dispose-client "MIDIClientDispose") :int
  "Disposes a MIDIClient object."
  (client object-ref))

;;; MIDIPorts
(cffi:defcfun (create-input-port "MIDIInputPortCreate") :int
  "Creates an input port through which the client may receive incoming MIDI messages from any MIDI source."
  (client object-ref)
  (portname :pointer)
  (read-proc :pointer)
  (ref-con :pointer)
  (port-ref :pointer))


(cffi:defcfun (create-output-port "MIDIOutputPortCreate") :int
  "Creates an output port through which the client may send outgoing MIDI messages to any MIDI destination."
  (client object-ref)
  (portname :pointer)
  (port-ref :pointer))

(cffi:defcfun (connect-source "MIDIPortConnectSource") :int
  "Establishes a connection from a source to a client's input port."
  (port object-ref)
  (source object-ref)
  (ref-con :pointer))

(cffi:defcfun (disconnect-source "MIDIPortDisconnectSource") :int
  "Closes a previously-established source-to-input port connection."
  (port object-ref)
  (source object-ref))

(cffi:defcfun (dispose-port "MIDIPortDispose") :int
  "Disposes a MIDIPort object."
  (port object-ref))

;;; with-cfstring
(defconstant +k-cf-string-encoding-utf-8+ #x08000100)

(defmacro with-cf-strings (bindings &body body)
  `(let ,(mapcar (lambda (bind) (list (car bind) nil)) bindings)
     (unwind-protect
	  (progn
	    ,@(loop for form in bindings collect
		    `(setf ,(car form) (cffi:foreign-funcall
					"CFStringCreateWithCString"
					:pointer (cffi:foreign-funcall "CFAllocatorGetDefault"
								       :pointer)
					:string ,(second form)
					:int +k-cf-string-encoding-utf-8+
					:pointer)))
	    ,@body)
       ,@(loop for form in bindings
	       collect `(cffi:foreign-funcall "CFRelease" :pointer ,(car form))))))

;;; Property of MIDI-OBJECT
(defun midiobject-display-name (midiobject)
  "Returns the name of a given midiobject."
  (cffi:with-foreign-objects ((cfstring :pointer)
			      (char :char 1024))
    (with-cf-strings ((property "displayName"))
      (cffi:foreign-funcall "MIDIObjectGetStringProperty"
			    :int midiobject
			    :pointer property
			    :pointer cfstring
			    :int)
      (cffi:foreign-funcall "CFStringGetCString" :pointer (cffi:mem-ref cfstring :pointer)
						 :pointer char
						 :int 1024
						 :int +k-cf-string-encoding-utf-8+)
      (cffi:foreign-funcall "CFRelease" :pointer (cffi:mem-ref cfstring :pointer))
      (cffi:foreign-string-to-lisp char :encoding :utf-8))))



;;; MIDIPacket

(cffi:defcstruct +midi-packet+
  (time-stamp-high :unsigned-int)
  (time-stamp-low :unsigned-int)
  (length :unsigned-short)
  (data :unsigned-char :count 256))

(cffi:defcstruct +midi-packet-list+
  (num-packets :unsigned-int)
  (packet (:struct +midi-packet+) :count 1))

(cffi:defcfun (packet-list-init "MIDIPacketListInit") :pointer
  "Prepares a MIDIPacketList to be built up dynamically."
  (pktlist :pointer))

(cffi:defcfun (packet-list-add "MIDIPacketListAdd") :pointer
  "Add a MIDI event to a MIDIPacketList."
  (pktlist :pointer)
  (limit-size :int)
  (packet :pointer)
  (time :unsigned-long-long)
  (data-size :int)
  (data :pointer))


;;; MIDI I/O
(cffi:defcfun "MIDISend" :int
  "Sends MIDI to a destination."
  (port object-ref)
  (destination object-ref)
  (pktlist :pointer))

(cffi:defcfun (midi-received "MIDIReceived") :int
  "Distributes incoming MIDI from a source to the client input ports which are connected to that source."
  (src object-ref)
  (mktlist :pointer))

(cffi:defcfun (midi-restart "MIDIRestart") :int
  "Stops and restarts MIDI I/O. This is useful for forcing CoreMIDI to ask its dirvers to rescan for hardware.")
