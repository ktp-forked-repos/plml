# ---------- CONFIGURATION ----------
# comment in and rebuild to debug mutexes
#CFLAGS+=-DDEBUG

# comment in and rebuild to get remove mutex protection from engine access
#CFLAGS+=-DNOLOCK
# ------ END OF CONFIGURATION -------

MATLAB=$(shell matlab -e | grep MATLAB= | sed -e "s/MATLAB=//")
MLARCH=$(shell matlab -e | grep ^ARCH= | sed -e "s/ARCH=//")
COMPILER=$(shell if (gcc --version 2>/dev/null | grep clang > /dev/null); then echo clang; else echo gcc; fi)

TARGET=plml
SOBJ=$(PACKSODIR)/$(TARGET).$(SOEXT)
TARGET2=plml2
SOBJ2=$(PACKSODIR)/$(TARGET2).$(SOEXT)
CFLAGS+=-I$(MATLAB)/extern/include
LIBDIR=$(MATLAB)/bin/$(MLARCH)
LIBS=-L$(LIBDIR) -leng -lmx

ifeq ($(COMPILER),clang)
	POST_LDFLAGS=-rpath $(LIBDIR) $(LIBS)$
else
	POST_LDFLAGS=-Wl,-rpath $(LIBDIR) $(LIBS) # -rpath=$(LIBDIR)?
endif

$(SOBJ): cpp/$(TARGET).o
	mkdir -p $(PACKSODIR)
	$(LD) $(LDSOFLAGS) -o $@ $(SWISOLIB) $< -lstdc++ $(POST_LDFLAGS)
	strip -x $@

$(SOBJ2): cpp/$(TARGET2).o
	mkdir -p $(PACKSODIR)
	$(LD) $(LDSOFLAGS) -o $@ $(SWISOLIB) $< -lstdc++ $(POST_LDFLAGS)
	strip -x $@

.SUFFIXES: .cpp .o
all: $(SOBJ) $(SOBJ2)

.cpp.o:
	$(CC) $(CFLAGS) -o $@ -c $<

check::
install:
	chmod +x scripts/logio
	chmod +x scripts/swiplml
	chmod +x scripts/fixdylibs
clean:
	rm -f c/$(TARGET).o c/$(TARGET2).o

distclean: clean
	rm -f $(SOBJ) $(SOBJ2)

fixdylibs:
	sudo scripts/fixdylibs

install-me:
	swipl -f none -g "pack_install('file:.',[upgrade(true)]), halt"

publish:
	swipl -f none -g "pack_property(plml,download(D)), pack_install(D), halt"
