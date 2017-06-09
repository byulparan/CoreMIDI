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

;; #### FIXME: elsewhere
(defun now ()
  (* 1.0d-9 #+ccl(ccl::current-time-in-nanoseconds)
	    #-ccl(cffi:foreign-funcall "mach_absolute_time" :int64)))


;; ==========================================================================
;; Data Types
;; ==========================================================================

(cffi:defctype object-ref :unsigned-int
  "The base class of many CoreMIDI objects.")

(cffi:defctype device-ref :unsigned-int
  "A MIDI device or external device, containing entities.")

(cffi:defctype entity-ref :unsigned-int
  "A MIDI entity, owned by a device, containing endpoints.")

(cffi:defctype endpoint-ref :unsigned-int
  "A MIDI source or destination, owned by an entity.")

(cffi:defctype client-ref :unsigned-int
  "An object maintaining per-client state.")

(cffi:defctype port-ref :unsigned-int
  "A MIDI connection port owned by a client.")


;; ==========================================================================
;; MIDI Devices
;; ==========================================================================

(cffi:defcfun (device-get-entity "MIDIDeviceGetEntity") entity-ref
  "Returns one of a given device's entities."
  (device device-ref)
  (entity-index-0 :int))

(cffi:defcfun (device-get-number-of-entities "MIDIDeviceGetNumberOfEntities")
    :int
  "Returns the number of entities in a given device."
  (device device-ref))

(cffi:defcfun (get-device "MIDIGetDevice") device-ref
  "Returns one of the devices in the system."
  (device-index-0 :int))

(cffi:defcfun (get-number-of-devices "MIDIGetNumberOfDevices") :int
  "Returns the number of devices in the system.")


;; ==========================================================================
;; MIDI Entities
;; ==========================================================================

(cffi:defcfun (entity-get-destination "MIDIEntityGetDestination") endpoint-ref
  "Returns one of a given entity's destinations."
  (entity entity-ref)
  (dest-index-0 :int))

(cffi:defcfun (entity-get-device "MIDIEntityGetDevice") :int
  "Returns an entity's device."
  (in-entity entity-ref)
  (out-device :pointer))

(cffi:defcfun
    (entity-get-number-of-destinations "MIDIEntityGetNumberOfDestinations")
    :int
  "Returns the number of destinations in a given entity."
  (entity entity-ref))

(cffi:defcfun
    (entity-get-number-of-sources "MIDIEntityGetNumberOfSources")
    :int
  "Returns the number of sources in a given entity."
  (entity entity-ref))

(cffi:defcfun (entity-get-source "MIDIEntityGetSource") endpoint-ref
  "Returns one of a given entity's sources"
  (entity entity-ref)
  (source-index-0 :int))


;; ==========================================================================
;;; MIDI Endpoints
;; ==========================================================================

(cffi:defcfun (destination-create "MIDIDestinationCreate") :int
  "Creates a virtual destination in client."
  (client client-ref)
  (name :pointer)
  (read-proc :pointer)
  (ref-con :pointer)
  (out-dest :pointer))

(cffi:defcfun (endpoint-dispose "MIDIEndpointDispose") :int
  "Disposes a virtual source or destination your client created."
  (endpt endpoint-ref))

(cffi:defcfun (endpoint-get-entity "MIDIEndpointGetEntity") :int
  "Returns an endpoint's entity."
  (in-endpoint endpoint-ref)
  (out-entity :pointer))

(cffi:defcfun (get-destination "MIDIGetDestination") endpoint-ref
  "Returns one of the destinations in the system."
  (dest-index-0 :int))

(cffi:defcfun (get-number-of-destinations "MIDIGetNumberOfDestinations") :int
  "Returns the number of destinations in the system.")

(cffi:defcfun (get-number-of-sources "MIDIGetNumberOfSources") :int
  "Returns the number of sources in the system.")

(cffi:defcfun (get-source "MIDIGetSource") endpoint-ref
  "Returns one of the sources in the system."
  (source-index-0 :int))

(cffi:defcfun (source-create "MIDISourceCreate") :int
  "Creates a virtual source in a client."
  (client client-ref)
  (name :pointer)
  (out-src :pointer))


;; ==========================================================================
;; MIDI Clients
;; ==========================================================================

(cffi:defcfun (client-create "MIDIClientCreate") :int
  "Creates a MIDIClient object."
  (name :pointer)
  (notify-proc :pointer)
  (notify-ref-con :pointer)
  (out-client :pointer))

(cffi:defcfun (client-dispose "MIDIClientDispose") :int
  "Disposes a MIDIClient object."
  (client client-ref))


;; ==========================================================================
;; MIDI Ports
;; ==========================================================================

(cffi:defcfun (input-port-create "MIDIInputPortCreate") :int
  "Creates an input port.
The client may receive incoming MIDI messages from any MIDI source."
  (client client-ref)
  (port-name :pointer)
  (read-proc :pointer)
  (ref-con :pointer)
  (in-port :pointer))

(cffi:defcfun (output-port-create "MIDIOutputPortCreate") :int
  "Creates an output port.
The client may send outgoing MIDI messages to any MIDI destination."
  (client client-ref)
  (port-name :pointer)
  (out-port :pointer))

(cffi:defcfun (port-connect-source "MIDIPortConnectSource") :int
  "Establishes a connection from a source to a client's input port."
  (port port-ref)
  (source endpoint-ref)
  (conn-ref-con :pointer))

(cffi:defcfun (port-disconnect-source "MIDIPortDisconnectSource") :int
  "Closes a previously-established source-to-input port connection."
  (port port-ref)
  (source endpoint-ref))

(cffi:defcfun (port-dispose "MIDIPortDispose") :int
  "Disposes a MIDIPort object."
  (port port-ref))


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


(cffi:defcstruct midi-notification
  (message-id :int)
  (message-size :unsigned-int))

