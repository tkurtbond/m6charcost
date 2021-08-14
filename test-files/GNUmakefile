TESTS=test.json breachworld.json kids-pcs-3.json kids-pcs-3.yaml


tests: \
	$(foreach T,$(filter %.json,$(TESTS)),output/$(basename $(T))-json.cost) \
	$(foreach T,$(filter %.yaml,$(TESTS)),output/$(basename $(T))-yaml.cost) \
	$(foreach T,$(filter %.json,$(TESTS)),output/$(basename $(T))-json.rst) \
	$(foreach T,$(filter %.yaml,$(TESTS)),output/$(basename $(T))-yaml.rst) \
	$(foreach T,$(filter %.json,$(TESTS)),output/$(basename $(T))-json.ms) \
	$(foreach T,$(filter %.yaml,$(TESTS)),output/$(basename $(T))-yaml.ms) 

output/%-json.cost : %.json
	../sm6 $< >$@

output/%-yaml.cost : %.yaml
	../sm6 $< >$@


output/%-json.rst : %.json
	../sm6rst -u @ -s -. -s '~#' $< >$@

output/%-yaml.rst : %.yaml
	../sm6rst -u @ -s -. -s '~#' $< >$@


output/%-json.ms : %.json
	../sm6troff-ms $< >$@

output/%-yaml.ms : %.yaml
	../sm6troff-ms $< >$@

clean:
	-rm output/*
