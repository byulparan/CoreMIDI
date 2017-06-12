(defpackage #:midi
  (:use #:cl)
  (:export
    ;; From cffi.lisp:
    ;; Data Types
    #:object-ref
    #:device-ref
    #:entity-ref
    #:endpoint-ref
    ;; MIDI Devices:
    #:device-get-entity
    #:device-get-number-of-entities
    #:get-device
    #:get-number-of-devices
    ;; MIDI Entities
    #:entity-get-destination
    #:entity-get-device
    #:entity-get-number-of-destinations
    #:entity-get-number-of-sources
    #:entity-get-source
    ;; MIDI Endpoints:
    #:destination-create
    #:endpoint-dispose
    #:endpoint-get-entity
    #:get-destination
    #:get-number-of-destinations
    #:get-number-of-sources
    #:get-source
    #:source-create
    ;; MIDI Clients:
    #:client-create
    #:client-dispose
    ;; MIDI Ports:
    #:input-port-create
    #:output-port-create
    #:port-connect-source
    #:port-disconnect-source
    #:port-dispose
    ;; MIDI Packet Lists:
    #:packet-list-add
    #:packet-list-init
    #:packet-next

    #:initialize
    #:midi-restart
    #:midiobject-display-name
    #:find-source
    #:find-destination
    #:list-of-sources
    #:list-of-destinations
    #:set-midi-callback
    #:midi-send
    #:midi-send-at

    #:add-midi-notify-callback
    #:set-midi-notify-callback
    #:+setup-changed+
    #:+object-added+
    #:+object-removed+
    #:+property-changed+
    #:+thru-connections-changed+
    #:+serial-port-owner-changed+
    #:+io-error+))
