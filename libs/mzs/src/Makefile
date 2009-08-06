
# Builds MzScheme and MrEd (if enabled), and
#  drives the main install

srcdir = .
prefix = /usr/local
exec_prefix = ${prefix}
bindir = ${exec_prefix}/bin
libdir = ${exec_prefix}/lib
includepltdir = ${prefix}/include/plt
libpltdir = ${exec_prefix}/lib/plt
collectsdir = ${exec_prefix}/lib/plt/collects
mandir = ${prefix}/man
docdir = ${prefix}/share/plt/doc
builddir = .

ALLDIRINFO = "$(DESTDIR)$(bindir)" \
             "$(DESTDIR)$(collectsdir)" \
             "$(DESTDIR)$(docdir)" \
             "$(DESTDIR)$(libdir)" \
             "$(DESTDIR)$(includepltdir)" \
             "$(DESTDIR)$(libpltdir)" \
             "$(DESTDIR)$(mandir)"

all:
	$(MAKE) 3m

3m:
	cd mzscheme; $(MAKE) 3m
	$(MAKE) no-3m

mred-3m:
	cd mred; $(MAKE) 3m

no-3m:
	cd .

cgc:
	cd mzscheme; $(MAKE) cgc
	$(MAKE) no-cgc

mred-cgc:
	cd mred; $(MAKE) cgc

no-cgc:
	cd .

both:
	$(MAKE) cgc
	$(MAKE) 3m

# Install (common) ----------------------------------------

SETUP_ARGS = -mvqX "$(DESTDIR)$(collectsdir)" -M setup

install:
	$(MAKE) install-3m

install-common-first:
	mkdir -p $(ALLDIRINFO)

install-common-middle:
	$(MAKE) copytree-run
	$(MAKE) install-no-post-collects
	$(MAKE) lib-finish

install-common-last:
	$(MAKE) fix-paths

lib-finish:
	echo "$(libdir)"

no-run:
	cd .

copytree-run:
	mzscheme/mzschemecgc -mvxqu \
          "$(srcdir)/../collects/setup/unixstyle-install.ss" \
          make-install-copytree "$(srcdir)/.." \
          $(ALLDIRINFO) "no"

install-mred-post-collects:
	cd mred; $(MAKE) install-post-collects

install-no-post-collects:
	cd .

fix-paths:
	if [ "$(DESTDIR)" != "" ]; then \
          mzscheme/mzschemecgc -mvxqu \
            "$(srcdir)/../collects/setup/unixstyle-install.ss" \
            make-install-destdir-fix "$(srcdir)/.." \
            $(ALLDIRINFO) "no"; \
        fi

# 3m install ----------------------------------------

install-3m:
	$(MAKE) install-common-first
	cd mzscheme; $(MAKE) install-3m
	$(MAKE) install-no-3m
	$(MAKE) install-common-middle
	mzscheme/mzscheme3m $(SETUP_ARGS)
	$(MAKE) install-common-last

install-no-3m:
	cd .

install-mred-3m:
	cd mred; $(MAKE) install-3m

# CGC install ----------------------------------------

install-cgc:
	$(MAKE) install-common-first
	cd mzscheme; $(MAKE) install-cgc
	$(MAKE) install-no-cgc
	$(MAKE) install-common-middle
	mzscheme/mzschemecgc $(SETUP_ARGS)
	$(MAKE) install-common-last

install-no-cgc:
	cd .

install-mred-cgc:
	cd mred; $(MAKE) install-cgc

# Both install ----------------------------------------

install-both:
	$(MAKE) install-common-first
	cd mzscheme; $(MAKE) install-both
	$(MAKE) install-no-both
	$(MAKE) install-common-middle
	mzscheme/mzscheme3m $(SETUP_ARGS)
	$(MAKE) install-common-last

install-no-both:
	cd .

install-mred-both:
	cd mred; $(MAKE) install-both

# Clean ----------------------------------------

clean:
	cd mzscheme; $(MAKE) clean
	if [ -d mred ]; then cd mred; $(MAKE) clean; fi
