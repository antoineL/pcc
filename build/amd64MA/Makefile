# Need some Unix shell...

ALL_SUBDIRS=	cc cpp ccom cxxcom ccom-sf
DIST_SUBDIRS=	$(ALL_SUBDIRS) driver f77 fcom csu libpcc libsoftfloat

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
