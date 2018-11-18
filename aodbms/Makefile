COBC=cobc
COBFLAGS=-std=mf -Wall -Werror -O2

LDFLAGS=-lzmq

CBL_SOURCE=src/aodbms.cbl
COB_SOURCES=

aodbms:
	$(COBC) $(COBFLAGS) -x -o aodbms $(CBL_SOURCE) $(COB_SOURCES) $(LDFLAGS)

install:
	mkdir -p $(out)/bin
	mv aodbms $(out)/bin
