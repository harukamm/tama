ORIGS := util.ml types.ml tokenize.ml #parse.ml
ORIGS := $(ORIGS:%.ml=src/%.ml)
STAGED := $(ORIGS:src/%.ml=test/%.ml)
SOURCES := $(STAGED)
CMOS := $(SOURCES:test/%.ml=test/%.cmo)
# RESULT := tama_test

all: $(CMOS)
	ocaml -I test -init test/test.ml

clean:
	rm -f test/*.cmi test/*.cmo

$(RESULT): $(CMOS)
	ocamlc $^ -o $@

$(CMOS): test/%.cmo: test/%.ml $(STAGED) | test/._d
	ocamldep $< > test/._d/$*.d
	ocamlc -I test -c $<

test/._d:
	mkdir -p $@

$(STAGED): test/%.ml: src/%.ml
	cp $< $@.tmp && mv $@.tmp $@

-include test/._d/*.d
