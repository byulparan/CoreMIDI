
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
	   #:midi-send))
