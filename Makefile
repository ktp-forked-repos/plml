MATLAB=$(shell matlab -e | grep MATLAB= | sed -e "s/MATLAB=//")
MLARCH=$(shell matlab -e | grep ^ARCH= | sed -e "s/ARCH=//")

TARGET=plml
SOBJ=$(PACKSODIR)/$(TARGET).$(SOEXT)
CFLAGS+=-I$(MATLAB)/extern/include
LIBDIR=$(MATLAB)/bin/$(MLARCH)
LIBS=-L$(LIBDIR) -lstdc++ -leng -lmx

ifeq ($(SOEXT),dylib)
	EXTRA_LDFLAGS=-rpath $(LIBDIR)
else
	EXTRA_LDFLAGS=
endif

$(SOBJ): cpp/$(TARGET).o
	mkdir -p $(PACKSODIR)
	$(LD) $(EXTRA_LDFLAGS) $(LIBS) $(LDSOFLAGS) -o $@ $(SWISOLIB) $<
	ln -s $(LIBDIR)/libtbb.dylib $(PACKSODIR)
	strip -x $@

.SUFFIXES: .cpp .o
all: $(SOBJ)

.cpp.o:
	$(CC) $(CFLAGS) -o $@ -c $<

check::
install:
	chmod +x scripts/logio
	chmod +x scripts/swiplml
	chmod +x scripts/fixdylibs
clean:
	rm -f c/$(TARGET).o
distclean: clean
	rm -f $(SOBJ)

fixdylibs:
	sudo scripts/fixdylibs

install-me:
	swipl -g "pack_install(.), halt"

publish:
	swipl -g "pack_property(plml,download(D)), pack_install(D), halt"
