MATLAB=$(shell matlab -e | grep MATLAB= | sed -e "s/MATLAB=//")
MLARCH=$(shell matlab -e | grep ^ARCH= | sed -e "s/ARCH=//")

TARGET=plml
SOBJ=$(PACKSODIR)/$(TARGET).$(SOEXT)
CFLAGS+=-I$(MATLAB)/extern/include
LIBDIR=$(MATLAB)/bin/$(MLARCH)
LIBS=-L$(LIBDIR) -lstdc++ -leng -lmx

$(SOBJ): cpp/$(TARGET).o
	mkdir -p $(PACKSODIR)
	$(LD) -rpath $(LIBDIR) $(LDSOFLAGS) -o $@ $(SWISOLIB) $< $(LIBS)
	ln -s $(LIBDIR)/libtbb.dylib $(PACKSODIR)
	strip -x $@

.SUFFIXES: .cpp .o
all: $(SOBJ)

.cpp.o:
	$(CC) $(CFLAGS) -o $@ -c $<

check::
install::
clean:
	rm -f c/$(TARGET).o
distclean: clean
	rm -f $(SOBJ)

fixdylibs:
	sudo scripts/fixdylibs

