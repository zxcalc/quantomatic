################################
# variables you might want to set in your own profile setup...
################################
### your local install of PolyML and isaplib
# POLYML_HOME=/home/ldixon/local/polyml-cvs-version
ISAPLIB_SOURCES=../../isaplib

################################
# general variables
################################

# polyml executable
POLYML_HOME=$(shell ../tools/findpoly.sh)
POLYML=$(POLYML_HOME)/bin/poly
POLYFLAGS=-H 200 $(if $(FAST),--disable-tests) $(if $(V),--test-log-level=$(V))


################################
# dynamic variables checked on call to make. 
################################
ML_SRC_FILES = $(shell find . | grep ".ML$$" | grep -v "test/protocol/" | grep -v "test/PROTOCOLTEST.ML$$" | grep -v "toplevel/")
ISAP_ML_SRC_FILES = $(ISAPLIB_SOURCES)/Makefile $(shell find $(ISAPLIB_SOURCES)/* | grep ".ML$$")

################################
# Fixed file locations
################################
# quanto heap file
QUANTO_HEAP=heaps/quanto.heap
# binary file to produce
CORE_BIN=bin/quanto-core
ALL_BINS=$(CORE_BIN) bin/quanto-xml-to-json

################################
# Targets:
################################
default: all

$(QUANTO_HEAP): $(ML_SRC_FILES) $(ISAP_ML_SRC_FILES)
	@mkdir -p heaps
	POLYFLAGS="$(POLYFLAGS)" ../tools/poly-build-heap -p -o $@ ROOT.ML

heap: $(QUANTO_HEAP)

bin/%: toplevel/%.ML $(QUANTO_HEAP) $(shell find "toplevel/$%" | grep ".ML$$")
	@mkdir -p bin
	POLYFLAGS="$(POLYFLAGS)" ../tools/polyc -l $(QUANTO_HEAP) -o $@ $<

all: $(ALL_BINS)

protocol-tests: $(CORE_BIN)
	$(POLYML) $(POLYFLAGS) -q --use "test/PROTOCOLTEST.ML" --core "$(CORE_BIN)" --log "test/protocol-tests.log"

test check: protocol-tests

# startup an ML shell using the quanto heap
ml-shell: $(QUANTO_HEAP)
	@../tools/polyml-toplevel -l $<

run: ml-shell

# 
localclean:
	rm -f heaps/*.polyml-heap
	rm -f bin/*.o
	find . -type d -name .polysave | xargs rm -rf
	rm -f $(ALL_BINS)

clean: localclean
	$(MAKE) -C $(ISAPLIB_SOURCES) clean

veryclean: clean
vclean: veryclean
