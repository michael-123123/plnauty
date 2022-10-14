# 
# ###############################################
# make variables
# ###############################################
# 

# nauty settings
NAUTYBASE   	:= nauty26r12

#
# ###############################################
# compile all sources
# ###############################################
#

all: $(NAUTYBASE) pl-nauty

pl-nauty:
	(cd src && make all)

install: uninstall
	mkdir build
	cp src/pl-nauty.so build
	cp src/densenauty.pl build
	cp src/pl-gtools.pl build

uninstall:
	rm -rf build

.PHONY: all pl-nauty install uninstall

#
# ###############################################
# clean-*
# ###############################################
#

clean: clean-nauty clean-plnauty

clean-plnauty:
	(cd src && make clean)


clean-nauty: clean-$(NAUTYBASE)

clean-$(NAUTYBASE):
	rm -rf $(NAUTYBASE)

.PHONY: clean clean-settings clean-plnauty clean-nauty clean-$(NAUTYBASE)

# 
# ###############################################
# nauty
# ###############################################
# 

nauty: $(NAUTYBASE)

$(NAUTYBASE): clean-$(NAUTYBASE) FORCE
	(cd nauty ;		\
	 tar xvzf $@.tar.gz ;	\
	 mv $@ ..)
	(cd $@; ./configure CFLAGS=-fPIC; make -j)

FORCE:

.PHONY: nauty FORCE
