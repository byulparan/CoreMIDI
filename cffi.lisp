(in-package :midi)

#-ccl
(handler-case
    (let* ((dir (concatenate 'string
		  (namestring (asdf:system-source-directory :coremidi))
		  "C/"))
	   (file (concatenate 'string dir "libcoremidi_wrapper.dylib")))
      (unless (probe-file file)
	(uiop:run-program (concatenate 'string "make -C " dir) :output t))
      (cffi:load-foreign-library file))
  (error ())) ;; #### FIXME: WTF?

#+ccl
(progn
  (cffi:define-foreign-library coremidi (:darwin (:framework "CoreMIDI")))
  (cffi:use-foreign-library coremidi))


;; ==========================================================================
;; Data Types
;; ==========================================================================

(cffi:defctype object-ref :unsigned-int
  "The base class of many CoreMIDI objects.")

(cffi:defcstruct notification
  (message-id :int)
  (message-size :unsigned-int))

(cffi:defctype unique-id :int
  "A unique identifier for a MIDIObjectRef.")

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

(cffi:defctype time-stamp :unsigned-long-long
  "A host clock time.")

(cffi:defcstruct packet
  (time-stamp time-stamp)
  (length :unsigned-short)
  (data :unsigned-char :count 256))

(cffi:defcstruct packet-list
  (num-packets :unsigned-int)
  (packet (:struct packet) :count 1))



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


;; ==========================================================================
;; MIDI Packet Lists
;; ==========================================================================

(cffi:defcfun (packet-list-add "MIDIPacketListAdd") :pointer
  "Add a MIDI event to a MIDIPacketList."
  (pktlist :pointer)
  (list-size :int)
  (cur-packet :pointer)
  (time time-stamp)
  (n-data :int)
  (data :pointer))

(cffi:defcfun (packet-list-init "MIDIPacketListInit") :pointer
  "Prepares a MIDIPacketList to be built up dynamically."
  (pktlist :pointer))

(cffi:defcfun (packet-next "MIDIPacketNext") :pointer
  "Advances a MIDIPacket pointer.
The pointer is advanced to the MIDIPacket that immediately follows a given
packet in memory, for packets that are part of a MIDIPacketList array."
  (pkt :pointer))


;; ==========================================================================
;; MIDI Objects and Properties
;; ==========================================================================

(cffi:defcfun (object-find-by-unique-id "MIDIObjectFindByUniqueID") :int
  "Locates a device, external device, entity, or endpoint by its uniqueID."
  (in-unique-id unique-id)
  (out-object :pointer)
  (out-object-type :pointer))

;; Slightly higher level than the actual CoreMIDI function to manipulate lisp
;; strings directly. Note the absence of GET in the name.
(defun object-string-property (object-ref property)
  "Wrapper around MIDIObjectGetStringProperty.
Return the value of PROPERTY (a Lisp string) in OBJECT-REF as a Lisp string as
well."
  ;; #### FIXME: we need to handle errors.
  (cffi:with-foreign-objects ((cf-string :pointer)
			      (string :char 1024))
    (with-cf-strings ((cf-property property))
      (cffi:foreign-funcall "MIDIObjectGetStringProperty"
	object-ref object-ref
	:pointer cf-property
	:pointer cf-string
	:int)
      (cffi:foreign-funcall "CFStringGetCString"
	:pointer (cffi:mem-ref cf-string :pointer)
	:pointer string
	:int 1024
	:int +k-cf-string-encoding-utf-8+)
      (cffi:foreign-funcall "CFRelease"
	:pointer (cffi:mem-ref cf-string :pointer))
      (cffi:foreign-string-to-lisp string :encoding :utf-8))))

;; #### FIXME: implement the rest.


;; ==========================================================================
;; MIDI I/O
;; ==========================================================================

(cffi:defcfun (flush-output "MIDIFlushOutput") :int
  "Unschedules previously-sent packets."
  (dest endpoint-ref))

(cffi:defcfun (received "MIDIReceived") :int
  "Distributes incoming MIDI from a source.
MIDI is distributed to the client input ports which are connected to that
source."
  (src endpoint-ref)
  (pktlist :pointer))

;; #### NOTE: exception to the naming scheme, to avoid colliding with Common
;; Lisp's RESTART.
(cffi:defcfun (rescan "MIDIRestart") :int
  "Stops and restarts MIDI I/O.")

(cffi:defcfun (send "MIDISend") :int
  "Sends MIDI to a destination."
  (port port-ref)
  (destination endpoint-ref)
  (pktlist :pointer))

(cffi:defcfun (send-sysex "MIDISendSysex") :int
  "Sends a single system-exclusive event, asynchronously."
  (request :pointer))


;; ==========================================================================
;; MIDI External Devices
;; ==========================================================================

(cffi:defcfun (get-external-device "MIDIGetExternalDevice") device-ref
  "Returns one of the external devices in the system."
  (device-index-0 :int))

(cffi:defcfun
    (get-number-of-external-devices "MIDIGetNumberOfExternalDevices")
    :int
  "Returns the number of external MIDI devices in the system.")


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
