# Need some Unix shell...

ALL_SUBDIRS=	mingw i386nbsd minix minix31 darwin minix2 amd64 amd64MA \
	m68k vax nova arm armeb mips mipseb powerpc sparc64 hppa pdp11 pdp10

#DIST_SUBDIRS=	$(ALL_SUBDIRS) bin

all install clean:
	@for subdir in $(ALL_SUBDIRS); do \
		_nextdir_=$${_thisdir_+$$_thisdir_/}$$subdir; \
		echo "===> $$_nextdir_"; \
		(_thisdir_=$$_nextdir_; export _thisdir_; cd $$subdir && \
		    exec $(MAKE) $(MFLAGS) $@) || exit $$?; \
		echo "<=== $$_nextdir_"; \
	done

distclean:
	@for subdir in $(DIST_SUBDIRS); do \
		_nextdir_=$${_thisdir_+$$_thisdir_/}$$subdir; \
		echo "===> $$_nextdir_"; \
		(_thisdir_=$$_nextdir_; export _thisdir_; cd $$subdir && \
		    exec $(MAKE) $(MFLAGS) $@) || exit $$?; \
		echo "<=== $$_nextdir_"; \
	done
	rm -f Makefile
