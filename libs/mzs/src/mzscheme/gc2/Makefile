
srcdir = .
builddir = .
prefix = /usr/local
exec_prefix = ${prefix}
bindir = ${exec_prefix}/bin
libdir = ${exec_prefix}/lib
libpltdir = ${exec_prefix}/lib/plt
collectsdir = ${exec_prefix}/lib/plt/collects

# for version.mak:
mainsrcdir = ./../..
-include ./../version.mak

CC = gcc
CPP = gcc -E
CXX = g++
AR = ar
ARFLAGS = ruv
RANLIB = ranlib

CPPFLAGS =    -DNEWGC_BTC_ACCOUNT  -I$(builddir)/.. -I$(srcdir)/../include
CFLAGS = -g -O2 $(CPPFLAGS)  -Wall 
LIBS = -ldl -lm  -rdynamic 

DEF_COLLECTS_DIR = +D INITIAL_COLLECTS_DIRECTORY='"'"`cd $(srcdir)/../../../collects; pwd`"'"'

XFORM_SETUP = ../mzschemecgc -rq $(srcdir)/xform.ss --setup
XFORM_NOPRECOMP = $(XFORM_SETUP) --cpp "$(CPP) $(CPPFLAGS)" -o
XSRCDIR = xsrc
XFORM = env XFORM_USE_PRECOMP=$(XSRCDIR)/precomp.h $(XFORM_NOPRECOMP)
SRCDIR = $(srcdir)/../src

OBJS =  salloc.o \
	bignum.o \
	bool.o \
	builtin.o \
	char.o \
	complex.o \
	dynext.o \
	env.o \
	error.o \
	eval.o \
	file.o \
	fun.o \
	hash.o \
	image.o \
	jit.o \
	list.o \
	module.o \
	network.o \
	numarith.o \
	number.o \
	numcomp.o \
	numstr.o \
	port.o \
	portfun.o \
	print.o \
        rational.o \
	read.o \
	regexp.o \
	sema.o \
	setjmpup.o \
	string.o \
	struct.o \
	stxobj.o \
	symbol.o \
	syntax.o \
	thread.o \
	type.o \
	vector.o \
	foreign.o \
	gc2.o

XSRCS = $(XSRCDIR)/salloc.c \
	$(XSRCDIR)/bignum.c \
	$(XSRCDIR)/bool.c \
	$(XSRCDIR)/builtin.c \
	$(XSRCDIR)/char.c \
	$(XSRCDIR)/complex.c \
	$(XSRCDIR)/dynext.c \
	$(XSRCDIR)/env.c \
	$(XSRCDIR)/error.c \
	$(XSRCDIR)/eval.c \
	$(XSRCDIR)/file.c \
	$(XSRCDIR)/fun.c \
	$(XSRCDIR)/hash.c \
	$(XSRCDIR)/image.c \
	$(XSRCDIR)/jit.c \
	$(XSRCDIR)/list.c \
	$(XSRCDIR)/module.c \
	$(XSRCDIR)/network.c \
	$(XSRCDIR)/numarith.c \
	$(XSRCDIR)/numcomp.c \
	$(XSRCDIR)/number.c \
	$(XSRCDIR)/numstr.c \
	$(XSRCDIR)/port.c \
	$(XSRCDIR)/portfun.c \
	$(XSRCDIR)/print.c \
        $(XSRCDIR)/rational.c \
	$(XSRCDIR)/read.c \
	$(XSRCDIR)/regexp.c \
	$(XSRCDIR)/sema.c \
	$(XSRCDIR)/setjmpup.c \
	$(XSRCDIR)/string.c \
	$(XSRCDIR)/struct.c \
	$(XSRCDIR)/stxobj.c \
	$(XSRCDIR)/symbol.c \
	$(XSRCDIR)/syntax.c \
	$(XSRCDIR)/thread.c \
	$(XSRCDIR)/type.c \
	$(XSRCDIR)/vector.c \
	$(XSRCDIR)/foreign.c \
	$(XSRCDIR)/main.c

all: xsrc
	$(MAKE) xobjects

xsrcs: $(XSRCS)

xsrc:
	mkdir xsrc

xobjects: $(OBJS) main.o

XFORMDEP_NOPRECOMP = $(srcdir)/gc2.h $(srcdir)/xform.ss $(srcdir)/xform-mod.ss
XFORMDEP = $(XFORMDEP_NOPRECOMP) $(XSRCDIR)/precomp.h

$(XSRCDIR)/precomp.h : $(XFORMDEP_NOPRECOMP) $(srcdir)/precomp.c $(srcdir)/../src/schpriv.h $(srcdir)/../include/scheme.h \
                       $(srcdir)/../sconfig.h $(srcdir)/../uconfig.h $(srcdir)/../src/schvers.h $(srcdir)/../src/schemef.h \
                       $(srcdir)/../src/stypes.h 
	env XFORM_PRECOMP=yes $(XFORM_NOPRECOMP) $(XSRCDIR)/precomp.h $(srcdir)/precomp.c

$(XSRCDIR)/salloc.c: ../src/salloc.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/salloc.c $(SRCDIR)/salloc.c
$(XSRCDIR)/bignum.c: ../src/bignum.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/bignum.c $(SRCDIR)/bignum.c
$(XSRCDIR)/bool.c: ../src/bool.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/bool.c $(SRCDIR)/bool.c
$(XSRCDIR)/builtin.c: ../src/builtin.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/builtin.c $(SRCDIR)/builtin.c
$(XSRCDIR)/char.c: ../src/char.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/char.c $(SRCDIR)/char.c
$(XSRCDIR)/complex.c: ../src/complex.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/complex.c $(SRCDIR)/complex.c
$(XSRCDIR)/dynext.c: ../src/dynext.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/dynext.c $(SRCDIR)/dynext.c
$(XSRCDIR)/env.c: ../src/env.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/env.c $(SRCDIR)/env.c
$(XSRCDIR)/error.c: ../src/error.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/error.c $(SRCDIR)/error.c
$(XSRCDIR)/eval.c: ../src/eval.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/eval.c $(SRCDIR)/eval.c
$(XSRCDIR)/file.c: ../src/file.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/file.c $(SRCDIR)/file.c
$(XSRCDIR)/fun.c: ../src/fun.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/fun.c $(SRCDIR)/fun.c
$(XSRCDIR)/hash.c: ../src/hash.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/hash.c $(SRCDIR)/hash.c
$(XSRCDIR)/image.c: ../src/image.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/image.c $(SRCDIR)/image.c
$(XSRCDIR)/jit.c: ../src/jit.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/jit.c $(SRCDIR)/jit.c
$(XSRCDIR)/module.c: ../src/module.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/module.c $(SRCDIR)/module.c
$(XSRCDIR)/list.c: ../src/list.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/list.c $(SRCDIR)/list.c
$(XSRCDIR)/network.c: ../src/network.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/network.c $(SRCDIR)/network.c
$(XSRCDIR)/numarith.c: ../src/numarith.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/numarith.c $(SRCDIR)/numarith.c
$(XSRCDIR)/number.c: ../src/number.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/number.c $(SRCDIR)/number.c
$(XSRCDIR)/numcomp.c: ../src/numcomp.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/numcomp.c $(SRCDIR)/numcomp.c
$(XSRCDIR)/numstr.c: ../src/numstr.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/numstr.c $(SRCDIR)/numstr.c
$(XSRCDIR)/port.c: ../src/port.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/port.c $(SRCDIR)/port.c
$(XSRCDIR)/portfun.c: ../src/portfun.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/portfun.c $(SRCDIR)/portfun.c
$(XSRCDIR)/print.c: ../src/print.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/print.c $(SRCDIR)/print.c
$(XSRCDIR)/rational.c: ../src/rational.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/rational.c $(SRCDIR)/rational.c
$(XSRCDIR)/read.c: ../src/read.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/read.c $(SRCDIR)/read.c
$(XSRCDIR)/regexp.c: ../src/regexp.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/regexp.c $(SRCDIR)/regexp.c
$(XSRCDIR)/sema.c: ../src/sema.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/sema.c $(SRCDIR)/sema.c
$(XSRCDIR)/setjmpup.c: ../src/setjmpup.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/setjmpup.c $(SRCDIR)/setjmpup.c
$(XSRCDIR)/string.c: ../src/string.o $(XFORMDEP)
	$(XFORM_SETUP) --cpp "$(CPP) -I../src $(CPPFLAGS)" -o $(XSRCDIR)/string.c $(SRCDIR)/string.c
$(XSRCDIR)/struct.c: ../src/struct.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/struct.c $(SRCDIR)/struct.c
$(XSRCDIR)/stxobj.c: ../src/stxobj.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/stxobj.c $(SRCDIR)/stxobj.c
$(XSRCDIR)/symbol.c: ../src/symbol.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/symbol.c $(SRCDIR)/symbol.c
$(XSRCDIR)/syntax.c: ../src/syntax.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/syntax.c $(SRCDIR)/syntax.c
$(XSRCDIR)/thread.c: ../src/thread.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/thread.c $(SRCDIR)/thread.c
$(XSRCDIR)/type.c: ../src/type.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/type.c $(SRCDIR)/type.c
$(XSRCDIR)/vector.c: ../src/vector.o $(XFORMDEP)
	$(XFORM) $(XSRCDIR)/vector.c $(SRCDIR)/vector.c
$(XSRCDIR)/foreign.c: ../../foreign/foreign.o $(XFORMDEP)
	$(XFORM_SETUP) --cpp "$(CPP) $(CPPFLAGS) -I../../foreign/gcc/libffi/include -I${SRCDIR}/../../mzscheme/src" -o $(XSRCDIR)/foreign.c $(SRCDIR)/../../foreign/foreign.c
$(XSRCDIR)/main.c: ../main.o $(XFORMDEP)
	$(XFORM_NOPRECOMP) $(XSRCDIR)/main.c $(DEF_COLLECTS_DIR) $(srcdir)/../main.c


salloc.o: $(XSRCDIR)/salloc.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/salloc.c -o salloc.o
bignum.o: $(XSRCDIR)/bignum.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/bignum.c -o bignum.o
bool.o: $(XSRCDIR)/bool.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/bool.c -o bool.o
builtin.o: $(XSRCDIR)/builtin.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/builtin.c -o builtin.o
char.o: $(XSRCDIR)/char.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/char.c -o char.o
complex.o: $(XSRCDIR)/complex.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/complex.c -o complex.o
dynext.o: $(XSRCDIR)/dynext.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/dynext.c -o dynext.o
env.o: $(XSRCDIR)/env.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/env.c -o env.o
error.o: $(XSRCDIR)/error.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/error.c -o error.o
eval.o: $(XSRCDIR)/eval.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/eval.c -o eval.o
file.o: $(XSRCDIR)/file.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/file.c -o file.o
fun.o: $(XSRCDIR)/fun.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/fun.c -o fun.o
hash.o: $(XSRCDIR)/hash.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/hash.c -o hash.o
image.o: $(XSRCDIR)/image.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/image.c -o image.o
jit.o: $(XSRCDIR)/jit.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/jit.c -o jit.o
list.o: $(XSRCDIR)/list.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/list.c -o list.o
module.o: $(XSRCDIR)/module.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/module.c -o module.o
network.o: $(XSRCDIR)/network.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/network.c -o network.o
numarith.o: $(XSRCDIR)/numarith.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/numarith.c -o numarith.o
number.o: $(XSRCDIR)/number.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/number.c -o number.o
numcomp.o: $(XSRCDIR)/numcomp.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/numcomp.c -o numcomp.o
numstr.o: $(XSRCDIR)/numstr.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/numstr.c -o numstr.o
port.o: $(XSRCDIR)/port.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/port.c -o port.o
portfun.o: $(XSRCDIR)/portfun.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/portfun.c -o portfun.o
print.o: $(XSRCDIR)/print.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/print.c -o print.o
rational.o: $(XSRCDIR)/rational.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/rational.c -o rational.o
read.o: $(XSRCDIR)/read.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/read.c -o read.o
regexp.o: $(XSRCDIR)/regexp.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/regexp.c -o regexp.o
sema.o: $(XSRCDIR)/sema.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/sema.c -o sema.o
setjmpup.o: $(XSRCDIR)/setjmpup.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/setjmpup.c -o setjmpup.o
string.o: $(XSRCDIR)/string.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/string.c -o string.o
struct.o: $(XSRCDIR)/struct.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/struct.c -o struct.o
stxobj.o: $(XSRCDIR)/stxobj.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/stxobj.c -o stxobj.o
symbol.o: $(XSRCDIR)/symbol.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/symbol.c -o symbol.o
syntax.o: $(XSRCDIR)/syntax.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/syntax.c -o syntax.o
thread.o: $(XSRCDIR)/thread.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/thread.c -o thread.o
type.o: $(XSRCDIR)/type.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/type.c -o type.o
vector.o: $(XSRCDIR)/vector.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/vector.c -o vector.o
foreign.o: $(XSRCDIR)/foreign.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/foreign.c -o foreign.o
main.o: $(XSRCDIR)/main.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/main.c -o main.o

gc2.o: $(srcdir)/gc2.c $(srcdir)/newgc.c $(srcdir)/compact.c $(srcdir)/newgc.c $(srcdir)/gc2.h \
       $(srcdir)/vm_osx.c $(srcdir)/vm_mmap.c $(srcdir)/vm_osk.c $(srcdir)/vm.c\
       $(srcdir)/vm_memalign.c $(srcdir)/alloc_cache.c \
       $(srcdir)/page_range.c $(srcdir)/protect_range.c $(srcdir)/var_stack.c $(srcdir)/stack_comp.c \
       $(srcdir)/../utils/splay.c $(srcdir)/my_qsort.c $(srcdir)/backtrace.c \
       $(srcdir)/weak.c $(srcdir)/fnls.c $(srcdir)/../include/scheme.h $(srcdir)/../src/schpriv.h
	$(CC) $(CFLAGS) -c $(srcdir)/gc2.c -o gc2.o

FOREIGN_OBJS = ../../foreign/gcc/libffi/src/*.o ../../foreign/gcc/libffi/src/*/*.o
FOREIGN_LIB = ../../foreign/gcc/libffi/libffi.la
FOREIGN_OBJSLIB = $(FOREIGN_OBJS)

EXTRA_OBJS_T = ../src/gmp.o $(FOREIGN_OBJ) $(FOREIGN_LIB)
EXTRA_OBJS_L = ../src/gmp.o $(FOREIGN_OBJSLIB)

../libmzscheme3m.a: $(OBJS) $(EXTRA_OBJS_T)
	$(AR) $(ARFLAGS) ../libmzscheme3m.a $(OBJS) $(EXTRA_OBJS_L)
	$(RANLIB) ../libmzscheme3m.a

../mzscheme3m: main.o ../libmzscheme3m.a
	cd ..; gcc -o mzscheme3m  gc2/main.o libmzscheme3m.a  $(LIBS)
# The above "cd .." prevents a problem with libtool's generated script in --enable-shared mode,
#  at least for Mac OS X. Beware of changing LIBS or LDFLAGS to inclucde something with a relative
#  path.

MZFWMMM = PLT_MzScheme.framework/Versions/$(FWVERSION)_3m/PLT_MzScheme

$(MZFWMMM): ../libmzscheme3m.a
	mkdir -p "PLT_MzScheme.framework/Versions/$(FWVERSION)_3m"
	$(CC) -o $(MZFWMMM) -framework CoreFoundation -dynamiclib -all_load ../libmzscheme3m.a  $(LIBS)
	rm -f PLT_MzScheme.framework/PLT_MzScheme
	ln -s Versions/$(FWVERSION)_3m/PLT_MzScheme PLT_MzScheme.framework/PLT_MzScheme	

../mzscheme3m.osx: $(MZFWMMM) main.o
	$(CC) -o ../mzscheme3m  main.o -Wl,-headerpad_max_install_names -F. -framework PLT_MzScheme
	mkdir -p "../PLT_MzScheme.framework/Versions/$(FWVERSION)_3m"
	cp "PLT_MzScheme.framework/Versions/$(FWVERSION)_3m/PLT_MzScheme" "../PLT_MzScheme.framework/Versions/$(FWVERSION)_3m/PLT_MzScheme"
	/usr/bin/install_name_tool -change "PLT_MzScheme.framework/Versions/$(FWVERSION)_3m/PLT_MzScheme" "@executable_path/PLT_MzScheme.framework/Versions/$(FWVERSION)_3m/PLT_MzScheme" "../mzscheme3m"


clean:
	/bin/rm -f ../mzscheme3m *.o $(XSRCDIR)/*
	/bin/rm -rf xform-collects
	/bin/rm -rf PLT_MzScheme.framework

#--------------------------------------------------

test.o: $(srcdir)/test.c
	$(CC) $(CFLAGS) -c $(srcdir)/test.c -o test.o

gct: test.o gc2.o
	$(CC) -o gct test.o gc2.o
