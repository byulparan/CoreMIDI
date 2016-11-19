
(defpackage #:midi
  (:use #:cl)
  (:export #:coremidi-start
	   #:midiobject-name
	   #:get-source
	   #:get-destination
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
