PKG := coremidi_wrapper

SRC := $(PKG).m
OBJ := $(PKG).o
LIB := lib$(PKG).dylib

all: $(LIB)

$(LIB): $(OBJ)
	cc -dynamiclib -o $@ $< -framework CoreMIDI

clean:
	rm -f $(OBJ)

distclean: clean
	rm -f $(LIB)

.PHONY: all clean distclean
