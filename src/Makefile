# 
# ###############################################
# make variables
# ###############################################
# 

# pl settings
PLSETTINGS	:= pl.conf
# nauty settings
NAUTYBASE   	:= ../nauty26r3
# pl version info
PLVERSION  	:= $(shell swipl --dump-runtime-variables | grep -i PLVERSION | sed s/\"//g |  sed s/\;//g | cut -d= -f2)
DPLVERSION	:= -DPLVERSION=$(PLVERSION)
DPLNDEBUG	:= -DPL_NAUTY_DEBUG

#
# ###############################################
# compile all sources
# ###############################################
#

all: $(PLSETTINGS) pl-nauty

install:
	cp pl-nauty.so ../build

#
# ###############################################
# clean-*
# ###############################################
#

clean: clean-settings clean-plnauty


clean-settings:
	rm -f $(PLSETTINGS)


clean-plnauty:
	rm -f *.o *.so *.gch

.PHONY: clean clean-settings clean-plnauty

# 
# ###############################################
# various compilation targets
# ###############################################
# 

# 
# compile nauty modules
# pl-nauty.so is what's loaded in prolog later
# 

pl-nauty: pl-nauty-libs pl-nauty-compat pl-densenauty-opts pl-densenauty
	$(CC) -O4 -s -I $(PLBASE)/include -I ${NAUTYBASE} $(DPLVERSION) $(DPLNDEBUG) $(PLCFLAGS) -fPIC $(PLLDFLAGS) -shared -c pl-nauty.c
	$(CC) -O4 -s -I $(PLBASE)/include -I ${NAUTYBASE} $(DPLVERSION) $(DPLNDEBUG) $(PLCFLAGS) -fPIC $(PLLDFLAGS) -shared -o pl-nauty.so \
		pl-nauty.o pl-nauty-libs.o pl-nauty-compat.o pl-densenauty-opts.o pl-densenauty.o ${NAUTYBASE}/*.a 

pl-nauty-libs:
	$(CC) -O4 -s -I $(PLBASE)/include -I ${NAUTYBASE} $(DPLVERSION) $(DPLNDEBUG) $(PLCFLAGS) -fPIC $(PLLDFLAGS) -shared -c pl-nauty-graph-get.c
	$(CC) -O4 -s -I $(PLBASE)/include -I ${NAUTYBASE} $(DPLVERSION) $(DPLNDEBUG) $(PLCFLAGS) -fPIC $(PLLDFLAGS) -shared -c pl-nauty-graph-unify.c
	$(CC) -O4 -s -I $(PLBASE)/include -I ${NAUTYBASE} $(DPLVERSION) $(DPLNDEBUG) $(PLCFLAGS) -fPIC $(PLLDFLAGS) -shared -c pl-nauty-graph-conv.c
	$(CC) -O4 -s -I $(PLBASE)/include -I ${NAUTYBASE} $(DPLVERSION) $(DPLNDEBUG) $(PLCFLAGS) -fPIC $(PLLDFLAGS) -shared -c pl-nauty-lists.c
	ld -r -o pl-nauty-libs.o pl-nauty-graph-get.o pl-nauty-graph-unify.o pl-nauty-graph-conv.o pl-nauty-lists.o

pl-nauty-compat:
	$(CC) -O4 -s -I $(PLBASE)/include -I ${NAUTYBASE} $(DPLVERSION) $(DPLNDEBUG) $(PLCFLAGS) -fPIC $(PLLDFLAGS) -shared -c pl-nauty-compat.c

pl-densenauty-opts:
	$(CC) -O4 -s -I $(PLBASE)/include -I ${NAUTYBASE} $(DPLVERSION) $(DPLNDEBUG) $(PLCFLAGS) -fPIC $(PLLDFLAGS) -shared -c pl-densenauty-opts.c

pl-densenauty:
	$(CC) -O4 -s -I $(PLBASE)/include -I ${NAUTYBASE} $(DPLVERSION) $(DPLNDEBUG) $(PLCFLAGS) -fPIC $(PLLDFLAGS) -shared -c pl-densenauty.c

# 
# ###############################################
# prolog environment variables
# ###############################################
# 

$(PLSETTINGS):	
	swipl --dump-runtime-variables | sed s/\"//g |  sed s/\;//g > pl.conf

-include $(PLSETTINGS)
