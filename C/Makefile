src=coremidi_wrap.m
target=libcoremidi_wrap.dylib

all: $(target)

$(target): coremidi_wrap.o
	cc -dynamiclib -o $(target) coremidi_wrap.o -framework CoreMIDI

clean:
	rm $(target)
