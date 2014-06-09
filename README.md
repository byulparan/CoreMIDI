# CoreMIDI
**The wrapper of CoreMIDI Framework for Common Lisp**

#### require:

- MacOSX
- [Quicklisp](http://www.quicklisp.org)
- [ClozureCL](http://www.clozure.com/clozurecl.html) or [SBCL](http://www.sbcl.org) or [ECL](http://ecls.sourceforge.net)

#### install note:
	It include objective-C file. because MIDIinput-handle callback is invoked from a separate C thread
	It called foreign-thread callback.	Currently, It support only ClozureCL.
	so in SBCL,ECL used wrapper objC-library for resolve that problem.
	That objC library will build automatically when You load CoreMIDI lisp library.

#### usage:
- `coremidi-start`  load coremidi resources and initialize it. 
- `list-of-sources` print all midi sources in the system.
- `set-midi-callback` register callback function for midi-input message from source(currently it supoort :note-on, :note-off, :cc, :bend).
- `list-of-destinations`	print all midi destinations in the system.
- `midi-send` send midi message to destination

#### examples:
	
	(ql:quickload :coremidi)

	(midi:coremidi-start)

	(midi:set-midi-callback (midi:get-source 0) :note-on
			(lambda (chan note vel)
			  (format t "channel: ~d  notenum: ~d  velocity: ~d~%" chan note vel)))


	(midi:midi-send (midi:get-destination 0) (+ (cb:now) 2) :note-on 1 60 100)
