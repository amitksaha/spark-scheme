# Makefile for all foreign-related libraires

srcdir = .
builddir = .
CC = gcc
CFLAGS = -g -O2  -Wall     \
         -I$(builddir)/../mzscheme \
         -I$(srcdir)/../mzscheme/include -I$(srcdir)/../mzscheme/src \
         -Igcc/libffi/include
ARFLAGS = ruv

# compile sub-libraries & foreign.c
all:
	$(MAKE) foreign.o
	$(MAKE) gcc/libffi/libffi.la

# Causes objects to be generated in gcc/libffi/src/
#  in inconvenience mode:
gcc/libffi/libffi.la:
	cd gcc/libffi; $(MAKE) libffi.la

foreign.o: $(srcdir)/foreign.c \
           $(srcdir)/../mzscheme/include/scheme.h \
           $(srcdir)/../mzscheme/src/schemef.h \
           $(srcdir)/../mzscheme/src/schvers.h
	$(CC) $(CFLAGS) -c $(srcdir)/foreign.c -o foreign.o

clean:
	rm -f foreign.o
	cd gcc/libffi; $(MAKE) clean
