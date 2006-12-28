SUBPROJS=trainguess

.PHONY: all
all:
	$(MAKE) -f Makefile.train
	$(MAKE) -f Makefile.tagandtest
	
.PHONY: clean
clean:
	$(MAKE) -f Makefile.train clean
	$(MAKE) -f Makefile.tagandtest clean