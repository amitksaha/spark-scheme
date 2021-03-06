#
# Makefile for mzscheme
#

# dynlink is a script that uses the right set of link commands
DYNLINK = mzc --ld 

srcdir = .
prefix = /usr/local
exec_prefix = ${prefix}
libpltdir = ${exec_prefix}/lib/plt
builddir = .

MZSRC = $(srcdir)/../src

CC = gcc
PLAIN_CC = gcc
CFLAGS = -g -O2  -Wall  -I$(builddir)/.. -I$(srcdir)/../include -I$(srcdir)/../src 

dynlib:
	$(MAKE) ../mzdyn.o
	$(MAKE) ../starter

dynlib3m:
	$(MAKE) ../mzdyn3m.o

dynexample:
	$(MAKE) ../dynexmpl.so

HEADERS = $(builddir)/../mzconfig.h \
          $(srcdir)/../include/scheme.h $(srcdir)/../src/schemef.h \
          $(srcdir)/../sconfig.h $(srcdir)/../uconfig.h \
          $(srcdir)/../include/escheme.h $(srcdir)/../src/stypes.h \
          $(srcdir)/../src/schemex.h $(srcdir)/../src/schemexm.h $(srcdir)/../src/schvers.h

MZDYNDEP = ../mzdyn.o $(srcdir)/../include/ext.exp $(srcdir)/../include/mzscheme.exp

../mzdyn.o: $(srcdir)/mzdyn.c $(srcdir)/../src/schvers.h  $(HEADERS)
	$(PLAIN_CC) -fPIC $(CFLAGS) -c $(srcdir)/mzdyn.c -o ../mzdyn.o

../mzdyn3m.o: $(srcdir)/mzdyn.c $(srcdir)/../src/schvers.h  $(HEADERS)
	$(PLAIN_CC) -fPIC $(CFLAGS) -DMZ_PRECISE_GC -c $(srcdir)/mzdyn.c -o ../mzdyn3m.o

../dynexmpl.so: dynexmpl.o $(MZDYNDEP)
	$(DYNLINK) ../dynexmpl.so dynexmpl.o
dynexmpl.o: $(srcdir)/dynexmpl.c  $(HEADERS)
	$(PLAIN_CC) $(CFLAGS) -c $(srcdir)/dynexmpl.c -o dynexmpl.o

../starter: $(srcdir)/ustart.c
	$(PLAIN_CC) $(CFLAGS) -o ../starter $(srcdir)/ustart.c

# Cygwin ########################################

ILIBDIR = $(libpltdir)

cygwin-install:
	$(MAKE) cygwin-install-core
	$(MAKE) cygwin-install-cgc
	$(MAKE) cygwin-install-3m

cygwin-install-core:
	gcc -c $(srcdir)/init.cc
	gcc -c $(srcdir)/fixup.c
	cp $(srcdir)/mzdyn.def mzdynx.def
	dlltool --def mzdynx.def --output-exp mzdyn.exp
	cd ../..; mkdir -p $(ILIBDIR)/gcc
	cd ../..; cp mzscheme/dynsrc/mzdynx.def $(ILIBDIR)/gcc/mzdyn.def
	cd ../..; cp mzscheme/dynsrc/init.o $(ILIBDIR)/gcc
	cd ../..; cp mzscheme/dynsrc/fixup.o $(ILIBDIR)/gcc

cygwin-install-cgc:
	cd ../..; cp mzscheme/dynsrc/mzdyn.exp $(ILIBDIR)/gcc
	gcc -c -O2 -I$(builddir)/.. -I$(srcdir)/../include $(srcdir)/mzdyn.c
	gcc -c -O2 -DAS_MSVC_EXTENSION -I$(builddir)/.. -I$(srcdir)/../include -o mzdynw.o $(srcdir)/mzdyn.c
	cd ../..; cp mzscheme/dynsrc/mzdyn.o $(ILIBDIR)/gcc
	cd ../..; cp mzscheme/dynsrc/mzdynw.o $(ILIBDIR)/gcc

cygwin-install-3m:
	cd ../..; cp mzscheme/dynsrc/mzdyn.exp $(ILIBDIR)/gcc/mzdyn3m.exp
	gcc -c -O2 -DMZ_PRECISE_GC -I$(builddir)/.. -I$(srcdir)/../include $(srcdir)/mzdyn.c -o mzdyn3m.o
	gcc -c -O2 -DAS_MSVC_EXTENSION -DMZ_PRECISE_GC -I$(builddir)/.. -I$(srcdir)/../include $(srcdir)/mzdyn.c -o mzdynw3m.o
	cd ../..; cp mzscheme/dynsrc/mzdyn3m.o $(ILIBDIR)/gcc
	cd ../..; cp mzscheme/dynsrc/mzdynw3m.o $(ILIBDIR)/gcc

clean:
	/bin/rm -f *.o Makefile.bak
