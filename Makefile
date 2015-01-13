MATLAB=$(shell matlab -e | grep MATLAB= | sed -e "s/MATLAB=//")
MLARCH=$(shell matlab -e | grep ^ARCH= | sed -e "s/ARCH=//")

TARGET=plml
SOBJ=$(PACKSODIR)/$(TARGET).$(SOEXT)
CFLAGS+=-I$(MATLAB)/extern/include
LIBDIR=$(MATLAB)/bin/$(MLARCH)
LIBS=-lstdc++ -L$(LIBDIR) -leng -lmx

ifeq ($(SOEXT),dylib)
	EXTRA_LDFLAGS=-rpath $(LIBDIR)
else
	EXTRA_LDFLAGS=
endif

#-Xlinker -rpath -Xlinker $(LIBDIR)
$(SOBJ): cpp/$(TARGET).o
	mkdir -p $(PACKSODIR)
	$(LD) $(LDSOFLAGS) $(EXTRA_LDFLAGS) $(LIBS) -o $@ $(SWISOLIB) $< -lstdc++
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
