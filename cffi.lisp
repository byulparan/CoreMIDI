(in-package :midi)

#-ccl
(let* ((dir (su:cat (namestring (asdf/system:system-source-directory :coremidi)) "C/"))
       (file (su:cat dir "libcoremidi_wrap.dylib")))
  (unless (probe-file file)
    (su:run-program (su:cat "make -C " dir) :output t :wait t)) 
  (push dir cffi:*foreign-library-directories*))

(cffi:define-foreign-library coremidi
  (:darwin #+ccl (:framework "CoreMIDI")
	   #-ccl "libcoremidi_wrap.dylib"))
 
(cffi:use-foreign-library coremidi)

;;; ------------------------------------------------------------------------------
;;; CoreMIDI midisend timestamp use AudioGetCurrentHostTime().
;;; CCL has #'ccl::current-time-in-nanoseconds (== AudioGetCurrentHostTime())
#-ccl
(cffi:define-foreign-library coreaudio
  (:darwin (:framework "CoreAudio")))

#-ccl (cffi:use-foreign-library coreaudio)

(defun midihost-time ()
  (* 1.0d-9
     #+ccl(ccl::current-time-in-nanoseconds)
     #-ccl(cffi:foreign-funcall "AudioGetCurrentHostTime" :int64)))

;;; ------------------------------------------------------------------------------

;;; MIDIObjectRef
(cffi:defctype +MIDI-OBJECT-REF+ :unsigned-int
  "Almost data types of CoreMIDI is Derives from MIDIObjectRef.")


;;; MIDI Devices
(cffi:defcfun (number-of-devices "MIDIGetNumberOfDevices") :int
  "Returns the number of devices in the system.")

(cffi:defcfun (get-device "MIDIGetDevice") +MIDI-OBJECT-REF+
  "Returns one of the devices in the system."
  (index :int))

(cffi:defcfun (number-of-entities-in-device "MIDIDeviceGetNumberOfEntities") :int
  "Returns the number of entities in a given device."
  (device +MIDI-OBJECT-REF+))

(cffi:defcfun (get-entity-in-device "MIDIDeviceGetEntity") +MIDI-OBJECT-REF+
  "Returns one of a given device's entities."
  (device +MIDI-OBJECT-REF+)
  (index :int))

;;; MIDI Entities
(cffi:defcfun (get-device-of-entity "MIDIEntityGetDevice") :int
  "Returns an entity's device."
  (entity +MIDI-OBJECT-REF+)
  (device-ref :pointer))

(cffi:defcfun (number-of-destinations-in-entity "MIDIEntityGetNumberOfDestinations") :int
  "Returns the number of destinations in a given entity."
  (entity +MIDI-OBJECT-REF+))

(cffi:defcfun (number-of-sources-in-entity "MIDIEntityGetNumberOfSources") :int
  "Returns the number of sources in a given entity."
  (entity +MIDI-OBJECT-REF+))

(cffi:defcfun (get-source-in-entity "MIDIEntityGetSource") +MIDI-OBJECT-REF+
  "Returns one of a given entity's sources"
  (entity +MIDI-OBJECT-REF+)
  (index :int))

(cffi:defcfun (get-destination-in-entity "MIDIEntityGetDestination") +MIDI-OBJECT-REF+
  "Returns one of a given entity's destinations."
  (entity +MIDI-OBJECT-REF+)
  (index :int))

;;; MIDI Endpoints
;;; input-Endpoint is "Source" and output-Endpoint is "Destination"

(cffi:defcfun (number-of-destinations "MIDIGetNumberOfDestinations") :int
  "Returns the number of destinations in the system.")

(cffi:defcfun (number-of-sources "MIDIGetNumberOfSources") :int
  "Returns the number of sources in the system.")

(cffi:defcfun "MIDIGetDestination" +MIDI-OBJECT-REF+
  "Returns one of the destinations in the system."
  (index :int))

(cffi:defcfun "MIDIGetSource" +MIDI-OBJECT-REF+
  "Returns one of the sources in the system."
  (index :int))

(cffi:defcfun (get-entity-of-endpoint "MIDIEndpointGetEntity") :int
  "Returns an endpoint's entity."
  (endpoint +MIDI-OBJECT-REF+)
  (entity-ref :pointer))

(cffi:defcfun (create-destination "MIDIDestinationCreate") :int
  "Creates a virtual destination in client."
  (client +midi-object-ref+)
  (name :pointer)
  (read-proc :pointer)
  (ref-con :pointer)
  (out-dest :pointer))

(cffi:defcfun (create-source "MIDISourceCreate") :int
  "Creates a virtual source in a client."
  (client +midi-object-ref+)
  (name :pointer)
  (out-src :pointer))

(cffi:defcfun (dispose-endpoint "MIDIEndpointDispose") :int
  "Disposes a virtual source or destination your client created."
  (endpt +midi-object-ref+))



;;; MIDIClient
(cffi:defcfun (create-client "MIDIClientCreate") :int
  "Creates a MIDIClient object."
  (name :pointer)
  (notify-proc :pointer)
  (notify-ref-con :pointer)
  (client-ref :pointer))

(cffi:defcfun (dispose-client "MIDIClientDispose") :int
  "Disposes a MIDIClient object."
  (client +MIDI-OBJECT-REF+))

;;; MIDIPorts
(cffi:defcfun (create-input-port "MIDIInputPortCreate") :int
  "Creates an input port through which the client may receive incoming MIDI messages from any MIDI source."
  (client +MIDI-OBJECT-REF+)
  (portname :pointer)
  (read-proc :pointer)
  (ref-con :pointer)
  (port-ref :pointer))


(cffi:defcfun (create-output-port "MIDIOutputPortCreate") :int
  "Creates an output port through which the client may send outgoing MIDI messages to any MIDI destination."
  (client +MIDI-OBJECT-REF+)
  (portname :pointer)
  (port-ref :pointer))

(cffi:defcfun (connect-source "MIDIPortConnectSource") :int
  "Establishes a connection from a source to a client's input port."
  (port +MIDI-OBJECT-REF+)
  (source +MIDI-OBJECT-REF+)
  (ref-con :pointer))

(cffi:defcfun (disconnect-source "MIDIPortDisconnectSource") :int
  "Closes a previously-established source-to-input port connection."
  (port +MIDI-OBJECT-REF+)
  (source +MIDI-OBJECT-REF+))

(cffi:defcfun (dispose-port "MIDIPortDispose") :int
  "Disposes a MIDIPort object."
  (port +midi-object-ref+))

;;; with-cfstring
(defconstant +k-cf-string-encoding-utf-8+ #x08000100)

(defmacro with-cf-strings (bindings &body body)
  `(let ,(mapcar (lambda (bind) (list (car bind) nil)) bindings)
     (unwind-protect (progn
		       ,@(loop for form in bindings
			       collect `(setf ,(car form) (cffi:foreign-funcall "CFStringCreateWithCString"
										:pointer (cffi:foreign-funcall "CFAllocatorGetDefault"
													       :pointer)
										:string ,(second form)
										:int +k-cf-string-encoding-utf-8+
										:pointer)))
		       ,@body)
       ,@(loop for form in bindings
	       collect `(cffi:foreign-funcall "CFRelease" :pointer ,(car form))))))

;;; Property of MIDI-OBJECT
(defun midiobject-name (midiobject)
  "Returns the name of a given midiobject."
  (cffi:with-foreign-objects ((cfstring :pointer)
			      (char :char 1024))
    (with-cf-strings ((property "name"))
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
;(cffi:defctype +midi-time-stamp+ :unsigned-long)

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
  (port +midi-object-ref+)
  (destination +midi-object-ref+)
  (pktlist :pointer))

(cffi:defcfun (midi-received "MIDIReceived") :int
  "Distributes incoming MIDI from a source to the client input ports which are connected to that source."
  (src +midi-object-ref+)
  (mktlist :pointer))

(cffi:defcfun (midi-restart "MIDIRestart") :int
  "Stops and restarts MIDI I/O. This is useful for forcing CoreMIDI to ask its dirvers to rescan for hardware.")
