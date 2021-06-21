PKG := coremidi_wrapper

CFLAGS = -arch arm64 -arch x86_64
SRC := $(PKG).m
OBJ := $(PKG).o
LIB := lib$(PKG).dylib

all: $(LIB)

$(LIB): $(OBJ)
	cc -dynamiclib -o $@ $< -framework CoreMIDI -arch arm64 -arch x86_64

clean:
	rm -f $(OBJ)

distclean: clean
	rm -f $(LIB)

.PHONY: all clean distclean
