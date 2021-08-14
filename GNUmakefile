CP=cp

PROGRAMS=sm6 sm6fmt sm6rst sm6troff-ms yaml2scm json2scm
OTHER_PROGRAMS=nm6 gm6 # Ignore om6 since I'm not working it right now.

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
	-rm $(foreach P,$(PROGRAMS:%=%$(EXE)) $(OTHER_PROGRAMS:%=%$(EXE)),build/$(P))
