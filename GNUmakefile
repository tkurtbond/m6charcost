CP=cp

PROGRAMS=sm6 sm6fmt sm6rst sm6troff-ms
OTHER_PROGRAMS=nm6 gm6 # sm6fmt is obsolete.  ignore om6.native because ocaml on macOS is completely broken.

all: $(foreach P,$(PROGRAMS:%=%$(EXE)) $(OTHER_PROGRAMS:%=%$(EXE)),build/$(P)) 


#CSCFLAGS=-static

build/% : %.go
	go build -o $@ $<


build/% : %.scm
	csc $(CSCFLAGS) -o $@ $<


%.native : %.ml
	corebuild -pkg yojson $@


build/% : %.nim
	nim compile --hints:off --colors:off -o:$@ $^


INSTALLDIR=$(shell eval dir='~/local/bin'; echo $$dir)

#install: $(PROGRAMS:%=%$(EXE))
#	install -D -t $(INSTALLDIR) $^

$(INSTALLDIR)/% : build/%
	$(CP) $< $@

install: $(foreach p,$(PROGRAMS:%=%$(EXE)),$(INSTALLDIR)/$(p))

print:
	echo $(INSTALLDIR)
	echo $(foreach p,$(PROGRAMS:%=%$(EXE)),$(INSTALLDIR)/$(p))

clean:
	-rm $(PROGRAMS:%=%$(EXE)) $(OTHER_PROGRAMS:%=%$(EXE)) 
