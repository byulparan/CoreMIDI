target=libcoremidi_wrap.dylib

all: $(target)

$(target):
	cc -dynamiclib -o $(target) coremidi_wrap.m -framework CoreMIDI

clean:
	rm $(target)
