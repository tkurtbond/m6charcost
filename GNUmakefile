PROGRAMS=sm6 sm6rst sm6troff
OTHER_PROGRAMS=sm6fmt nm6 gm6 #ignore om6.native because ocaml on macOS is completely broken.

all: $(PROGRAMS:%=%$(EXE)) $(OTHER_PROGRAMS:%=%$(EXE)) 


#CSCFLAGS=-static

% : %.go
	go build -o $@ $<


% : %.scm
	csc $(CSCFLAGS) $^


%.native : %.ml
	corebuild -pkg yojson $@


% : %.nim
	nim compile --hints:off --colors:off $^

INSTALLDIR=$(shell eval dir='~/local/bin/'; echo $$dir)

install: $(PROGRAMS:%=%$(EXE))
	install -D -t $(INSTALLDIR) $^

clean:
	-rm $(PROGRAMS:%=%$(EXE)) $(OTHER_PROGRAMS:%=%$(EXE)) 
