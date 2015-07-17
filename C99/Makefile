
all: tisbl

tisbl: libtisbl.a driver.c getopt.c getopt.h
	$(CC) $(CFLAGS) -std=c99 -o tisbl driver.c getopt.c libtisbl.a -lm

libtisbl.a: tisbl.o stdlib.o
	$(AR) cru libtisbl.a tisbl.o stdlib.o

tisbl.o: tisbl.c tisbl.h
	$(CC) $(CFLAGS) -std=c99 -c tisbl.c

stdlib.o: stdlib.c tisbl.h
	$(CC) $(CFLAGS) -std=c99 -c stdlib.c

clean:
	$(RM) -f tisbl.o stdlib.o
	$(RM) -f libtisbl.a
	$(RM) -f tisbl

.PHONY: clean all

