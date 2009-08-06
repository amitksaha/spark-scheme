#
# Makefile for mzscheme
#

srcdir = .
prefix = /usr/local
exec_prefix = ${prefix}
bindir = ${exec_prefix}/bin
libdir = ${exec_prefix}/lib
includepltdir = ${prefix}/include/plt
libpltdir = ${exec_prefix}/lib/plt
collectsdir = ${exec_prefix}/lib/plt/collects
builddir = .

# for version.mak:
mainsrcdir = ./..
-include ./version.mak

CC = gcc
CPP = gcc -E

AR = ar
ARFLAGS = ruv
RANLIB = ranlib

MZSRC = $(srcdir)/src

FOREIGN_DIR = ../foreign
FOREIGN_OBJ = $(FOREIGN_DIR)/foreign.o
FOREIGN_OBJS = $(FOREIGN_OBJ) $(FOREIGN_DIR)/gcc/libffi/src/*.o $(FOREIGN_DIR)/gcc/libffi/src/*/*.o
FOREIGN_LIB = $(FOREIGN_OBJ) ../foreign/gcc/libffi/libffi.la
FOREIGN_OBJSLIB = $(FOREIGN_OBJS)
# Dependency always uses FOREIGN_LIB, but linker uses FOREIGN_OBJSLIB. The
#  FOREIGN_OBJSLIB expansion has too many "*"s to be a valid make target,
#  while FOREIGN_LIB is not always acceptable to the linker.
# The foreign build always uses libtool, so that's why we use .la instead of a
#  in FOREIGN_LIB.

LLVM_DIR = ../llvm

# About EXE_SUFFIX:
#  Cygwin offers the illusion that binaries have no ".exe" suffix by
#  having the file-open syscall open an ".exe" file for reading when
#  (1) the given filename has no suffix, and (2) there's not a suffixless
#  file present. However, Cygwin doesn't do that when opening a file for
#  output, even when the output mode is 'update. So we need to explcitly
#  add EXE_SUFFIX in a few places.

################################################################################

bin: 
	$(MAKE) normal-bin

normal-bin:
	$(MAKE) 3m

common:
	$(MAKE) g-c
	$(MAKE) foreign-stuff
	$(MAKE) llvm-nothing

cgc:
	$(MAKE) common
	$(MAKE) dynlib
	$(MAKE) mzlibrary
	$(MAKE) mzschemecgc 

3m:
	$(MAKE) cgc
	cd gc2; $(MAKE) all
	cd dynsrc; $(MAKE) dynlib3m
	cd gc2; $(MAKE) ../mzscheme3m

both:
	$(MAKE) cgc
	$(MAKE) 3m

oskit: 
	$(MAKE) common
	$(MAKE) mzlibrary
	$(MAKE) mzscheme.multiboot

mzschemelib: 
	$(MAKE) g-c 
	$(MAKE) foreign-stuff
	$(MAKE) mzlibrary 

dynlib:
	cd src; $(MAKE) mzheaders
	cd dynsrc; $(MAKE) dynlib 

dynexample:
	$(MAKE) dynlib
	cd dynsrc; $(MAKE) dynexample 

oe:
	$(MAKE) dynlib
	cd dynsrc; $(MAKE) oe

g-c:
	cd gc; $(MAKE) ../libmzgc.a 

sproc-extra-lib:
	cd gc; $(MAKE) sproc.o
	$(MAKE) sproc.o

gc.a: 
	$(AR) $(ARFLAGS) gc/gc.a gc/*.o

# Compilation of the foreign libraries (this compiles all of them)
foreign-stuff:
	cd $(FOREIGN_DIR); $(MAKE) all

foreign-nothing:
	echo no foreign

# llvm
llvm-stuff:
	cd $(LLVM_DIR); $(MAKE) all

llvm-nothing:
	echo no llvm

mzlibrary:
	cd src; $(MAKE) all 
	$(MAKE) libmzscheme.a

libmzscheme.a: src/*.o $(FOREIGN_OBJ) $(FOREIGN_LIB)
	$(AR) $(ARFLAGS) libmzscheme.a src/*.o $(FOREIGN_OBJSLIB) 
	$(RANLIB) libmzscheme.a

# Irix sproc, only
sproc.o: gc/sproc.o
	cp gc/sproc.o sproc.o

# Unix ----------------------------------------

mzschemecgc: libmzscheme.a libmzgc.a main.o $(SPECIALIZINGOBJECTS)
	gcc -o mzschemecgc main.o $(SPECIALIZINGOBJECTS) libmzscheme.a libmzgc.a  -ldl -lm  -rdynamic 

# Mac OS ----------------------------------------

MZFW = PLT_MzScheme.framework/Versions/$(FWVERSION)/PLT_MzScheme

$(MZFW): libmzscheme.a libmzgc.a $(SPECIALIZINGOBJECTS)
	mkdir -p  PLT_MzScheme.framework/Versions/$(FWVERSION)
	$(CC) -o $(MZFW) -framework CoreFoundation -dynamiclib -all_load $(SPECIALIZINGOBJECTS) libmzscheme.a libmzgc.a  -ldl -lm  -rdynamic 
	rm -f PLT_MzScheme.framework/PLT_MzScheme
	ln -s Versions/$(FWVERSION)/PLT_MzScheme PLT_MzScheme.framework/PLT_MzScheme

mzschemecgc.osx: $(MZFW) main.o
	$(CC) -o mzschemecgc  main.o -Wl,-headerpad_max_install_names -F. -framework PLT_MzScheme
	/usr/bin/install_name_tool -change "PLT_MzScheme.framework/Versions/$(FWVERSION)/PLT_MzScheme" "@executable_path/PLT_MzScheme.framework/Versions/$(FWVERSION)/PLT_MzScheme" "mzschemecgc"

# OSKit ----------------------------------------

mzscheme.multiboot : libmzscheme.a libmzgc.a main.o
	$(LD) -Ttext 100000 $(LDFLAGS) /lib/oskit/multiboot.o \
		-o mzscheme.multiboot main.o -L. -lmzscheme -lgc \
                -L/lib -loskit_kern -loskit_clientos  \
                -loskit_freebsd_c -loskit_c -loskit_lmm \
                -loskit_freebsd_m -loskit_memfs -loskit_fsnamespace  \
                -loskit_dev -loskit_com \
                -loskit_kern -loskit_freebsd_c -loskit_c \
		/lib/oskit/crtn.o

# ----------------------------------------

DEF_COLLECTS_DIR = -DINITIAL_COLLECTS_DIRECTORY='"'"`cd $(srcdir)/../../collects; pwd`"'"'

main.o: $(srcdir)/main.c $(srcdir)/include/scheme.h $(srcdir)/sconfig.h $(srcdir)/src/stypes.h $(srcdir)/cmdline.inc $(srcdir)/oskglue.inc
	$(CC) -g -O2  -Wall     $(DEF_COLLECTS_DIR) -I$(builddir) -I$(srcdir)/include -c $(srcdir)/main.c -o main.o

main_ee.o: main.o
	$(CC) -g -O2  -Wall     $(DEF_COLLECTS_DIR) -I$(builddir) -I$(srcdir)/include -DSTANDALONE_WITH_EMBEDDED_EXTENSION -c $(srcdir)/main.c -o main_ee.o

ee-main:
	$(MAKE) main_ee.o

ee-app: $(MZSCHEME) main_ee.o
	if [ "$(EEAPP)" = '' ] ; then echo "ERROR: You must specify EEAPP" ; else $(CC) $(MAIN_CFLAGS_NODBG) -o $(EEAPP) main_ee.o $(EEOBJECTS) $(SPECIALIZINGOBJECTS) libmzscheme.a libmzgc.a -ldl -lm  -rdynamic  ; fi

testext.o: testext.c
	$(CC) $(MAIN_CFLAGS) -I/home/scheme/plt/compiler/ -c testext.c -o testext.o

mzstart.exe: $(srcdir)/dynsrc/start.c
	$(CC) -g -O2 -o mzstart.exe $(srcdir)/dynsrc/start.c

starter: mzstart.exe

exn:
	$(MAKE) $(srcdir)/src/schexn.h

CSTARTUPDEST = cstartup.inc

startup:
	$(MAKE) $(srcdir)/src/$(CSTARTUPDEST)

mark:
	$(MAKE) $(srcdir)/src/mzmark.c

headers:
	./mzschemecgc -qr $(srcdir)/mkincludes.ss "$(DESTDIR)$(includepltdir)" "$(srcdir)" .

$(srcdir)/src/schexn.h: $(srcdir)/src/exnsrc.ss $(srcdir)/src/makeexn
	./mzschemecgc -gqr $(srcdir)/src/makeexn < $(srcdir)/src/exnsrc.ss > $(srcdir)/src/schexn.h

$(srcdir)/src/$(CSTARTUPDEST): $(srcdir)/src/startup.ss $(srcdir)/src/schvers.h $(srcdir)/src/schminc.h
	./mzschemecgc -qr $(srcdir)/src/sstoinc.ss $(CSTARTUPEXTRA) < $(srcdir)/src/startup.ss > $(srcdir)/src/$(CSTARTUPDEST)

$(srcdir)/src/mzmark.c: $(srcdir)/src/mzmarksrc.c $(srcdir)/src/mkmark.ss
	mzscheme -qr $(srcdir)/src/mkmark.ss < $(srcdir)/src/mzmarksrc.c > $(srcdir)/src/mzmark.c

cstartup.zo:
	$(MAKE) startup CSTARTUPEXTRA='zo' CSTARTUPDEST="../cstartup.zo"

clean:
	/bin/rm -rf tmp[123456789] tests/tmp[123456789] tests/sub[123]
	/bin/rm -f tests/*~
	/bin/rm -f *.o core
	/bin/rm -f mzdyn.o libmzgc.a libmzscheme.a libmzscheme3m.a libdl.a mzscheme mzscheme.multiboot
	/bin/rm -f include/macosxpre
	/bin/rm -f include/macosxpre.p
	cd gc; $(MAKE) clean
	cd gc2; $(MAKE) clean
	cd sgc; $(MAKE) clean
	cd src; $(MAKE) clean
	cd dynsrc; $(MAKE) clean
	if [ -d ../foreign ] ; then cd ../foreign ; $(MAKE) clean ; fi
	if [ -d ../llvm    ] ; then cd ../llvm    ; $(MAKE) clean ; fi

clean.osx:
	rm -rf PLT_MzScheme.framework
	$(MAKE) clean

BUILDINFO=$(DESTDIR)$(libpltdir)/buildinfo
ICP=cp

install:
	$(MAKE) install-3m

install-3m:
	$(MAKE) headers
	$(MAKE) unix-install
	$(MAKE) unix-install-3m
	$(MAKE) unix-install-3m-final

install-cgc:
	$(MAKE) headers
	$(MAKE) unix-install
	$(MAKE) unix-install-cgc
	$(MAKE) unix-install-cgc-final

install-both:
	$(MAKE) headers
	$(MAKE) unix-install
	$(MAKE) unix-install-3m
	$(MAKE) unix-install-cgc
	$(MAKE) unix-install-3m-final

# Unix install ----------------------------------------

# Prefix might be relative to srcdir, or it might be absolute, so we
# have to go up and install things from there.

unix-install:	
	cd ..; rm -f "$(DESTDIR)$(bindir)/mzschemecgc"
	cd ..; rm -f "$(DESTDIR)$(bindir)/mzscheme"
	cd ..; cp mzscheme/starter "$(DESTDIR)$(libpltdir)/starter"
	cd ..; echo 'CC=gcc' > "$(BUILDINFO)"
	cd ..; echo 'CFLAGS=-g -O2   -Wall' >> "$(BUILDINFO)"
	cd ..; echo 'OPTIONS=' >> "$(BUILDINFO)"
	cd ..; echo 'MZOPTIONS=' >> "$(BUILDINFO)"
	cd ..; echo "LDFLAGS=" >> "$(BUILDINFO)"
	cd ..; echo 'LIBS=-ldl -lm  -rdynamic ' >> "$(BUILDINFO)"

unix-install-cgc:
	cd ..; $(ICP) mzscheme/libmzgc.a "$(DESTDIR)$(libdir)/libmzgc.a"
	cd ..; $(ICP) mzscheme/libmzscheme.a "$(DESTDIR)$(libdir)/libmzscheme.a"
	cd ..; $(ICP) mzscheme/mzschemecgc "$(DESTDIR)$(bindir)/mzschemecgc"
	cd ..; cp mzscheme/mzdyn.o "$(DESTDIR)$(libpltdir)/mzdyn.o"
	./mzschemecgc -mvqu "$(srcdir)/collects-path.ss" "$(DESTDIR)$(bindir)/mzschemecgc" ${collectsdir}

unix-install-cgc-final:
	cd .

unix-install-3m:
	cd ..; $(ICP) mzscheme/mzscheme3m "$(DESTDIR)$(bindir)/mzscheme"
	cd ..; $(ICP) mzscheme/libmzscheme3m.a "$(DESTDIR)$(libdir)/libmzscheme3m.a"
	cd ..; $(ICP) mzscheme/mzdyn3m.o "$(DESTDIR)$(libpltdir)/mzdyn3m.o"
	./mzscheme3m -mvqu "$(srcdir)/collects-path.ss" "$(DESTDIR)$(bindir)/mzscheme" ${collectsdir}

unix-install-3m-final:
	cd .

# OS X install ----------------------------------------

MZFWDIR = /PLT_MzScheme.framework
MZFWMMM = PLT_MzScheme.framework/Versions/$(FWVERSION)_3m/PLT_MzScheme

osx-install:
	$(MAKE) unix-install
	rm -f $(MZFWDIR)/PLT_MzScheme
	rm -rf $(MZFWDIR)/Versions/$(FWVERSION)
	rm -rf $(MZFWDIR)/Versions/$(FWVERSION)_3m
	if [  = yes ] ; then rm -rf $(MZFWDIR) ; fi
	mkdir -p 

osx-install-cgc:
	$(MAKE) unix-install-cgc
	mkdir -p "$(MZFWDIR)/Versions/$(FWVERSION)"
	cp $(MZFW) $(MZFWDIR)/Versions/$(FWVERSION)/
	if [  = yes ] ; then /usr/bin/install_name_tool -change "@executable_path/PLT_MzScheme.framework/Versions/$(FWVERSION)/PLT_MzScheme" "@executable_path/../lib/PLT_MzScheme.framework/Versions/$(FWVERSION)/PLT_MzScheme" "$(bindir)/mzschemecgc" ; fi

osx-install-cgc-final:
	$(MAKE) unix-install-cgc-final
	ln -s Versions/$(FWVERSION)/PLT_MzScheme $(MZFWDIR)/

osx-install-3m:
	$(MAKE) unix-install-3m
	mkdir -p "$(MZFWDIR)/Versions/$(FWVERSION)_3m"
	cp $(MZFWMMM) $(MZFWDIR)/Versions/$(FWVERSION)_3m/
	if [  = yes ] ; then /usr/bin/install_name_tool -change "@executable_path/PLT_MzScheme.framework/Versions/$(FWVERSION)_3m/PLT_MzScheme" "@executable_path/../lib/PLT_MzScheme.framework/Versions/$(FWVERSION)_3m/PLT_MzScheme" "$(bindir)/mzscheme" ; fi

osx-install-3m-final:
	$(MAKE) unix-install-3m-final
	ln -s Versions/$(FWVERSION)_3m/PLT_MzScheme $(MZFWDIR)/


# Cygwin install ----------------------------------------

unix-cygwin-install:
	$(MAKE) unix-install
	cd dynsrc; $(MAKE) cygwin-install-core

unix-cygwin-install-cgc:
	$(MAKE) unix-install-cgc
	cd dynsrc; $(MAKE) cygwin-install-cgc

unix-cygwin-install-cgc-final:
	$(MAKE) unix-install-cgc-final

unix-cygwin-install-3m:
	$(MAKE) unix-install-3m
	cd dynsrc; $(MAKE) cygwin-install-3m

unix-cygwin-install-3m-final:
	$(MAKE) unix-install-3m-final
