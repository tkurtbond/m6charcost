SHELL=/bin/bash
CP=cp

ifndef CSC
CSC=$(shell type -p csc || type -p chicken-csc || echo 'echo "CSC does not exist; "')
endif

PROGRAMS=sm6 sm6fmt sm6rst sm6troff-ms
OTHER_PROGRAMS=nm6 gm6 # Ignore om6 since I'm not working it right now.
TEST_FILES=$(wildcard test-data/*.yaml) $(wildcard test-data/*.json)
TEST_FILES_COST=\
	$(patsubst test-data/%.yaml,build/test-data/%-yaml.cost,$(TEST_FILES)) \
	$(patsubst test-data/%.json,build/test-data/%-json.cost,$(TEST_FILES)) \
	$(patsubst test-data/%.yaml,build/test-data/%-yaml.fmt-cost,$(TEST_FILES)) \
	$(patsubst test-data/%.json,build/test-data/%-json.fmt-cost,$(TEST_FILES)) \
	$(patsubst test-data/%.yaml,build/test-data/%-yaml.gen.rst,$(TEST_FILES)) \
	$(patsubst test-data/%.json,build/test-data/%-json.gen.rst,$(TEST_FILES))

all: $(foreach P,$(PROGRAMS:%=%$(EXE)),build/$(P))

extras: $(foreach P,$(OTHER_PROGRAMS:%=%$(EXE)),build/$(P))

#CSCFLAGS=-static

build/% : %.go
	go build -o $@ $<


build/% : %.scm
	$(CSC) $(CSCFLAGS) -o $@ $<


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

build/test-data/%-yaml.cost : test-data/%.yaml
	sm6 $< >$@

build/test-data/%-json.cost : test-data/%.json
	sm6 $< >$@

build/test-data/%-yaml.fmt-cost : test-data/%.yaml
	sm6 $< >$@

build/test-data/%-json.fmt-cost : test-data/%.json
	sm6 $< >$@

build/test-data/%-yaml.gen.rst : test-data/%.yaml
	sm6rst $< >$@

build/test-data/%-json.gen.rst : test-data/%.json
	sm6rst $< >$@

test: $(TEST_FILES_COST)

print:
	echo $(INSTALLDIR)
	echo $(foreach p,$(PROGRAMS:%=%$(EXE)),$(INSTALLDIR)/$(p))

clean:
	-rm $(foreach P,$(PROGRAMS:%=%$(EXE)) $(OTHER_PROGRAMS:%=%$(EXE)),build/$(P))
