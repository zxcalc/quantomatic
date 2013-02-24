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
POLYML=$(POLYML_HOME)/bin/poly -H 200
POLYML_LIB=$(POLYML_HOME)/lib

################################
# dynamic variables checked on call to make. 
################################
ML_SRC_FILES = $(shell find . | grep ".ML$$" | grep -v "test/protocol/" | grep -v "PROTOCOLTEST.ML$$")
ISAP_ML_SRC_FILES = $(ISAPLIB_SOURCES)/Makefile $(shell find $(ISAPLIB_SOURCES) | grep ".ML$$")

################################
# HEAP files defined in make_heap.ML, make_bin.ML
################################
# quanto heap file
QUANTO_HEAP=heaps/quanto.polyml-heap
# binary file to produce
THE_BIN=quanto-core

################################
# Targets: 
################################
default: bin

$(QUANTO_HEAP):  $(ML_SRC_FILES) 
	echo 'use "make_heap.ML";' | $(POLYML)
	@echo "Built polyml heap: $(QUANTO_HEAP)"

heap: $(QUANTO_HEAP)

standard-tests: $(QUANTO_HEAP)
	echo 'use "TEST.ML";' | $(POLYML)
	@echo ""

protocol-tests: bin/$(THE_BIN)
	echo 'use "PROTOCOLTEST.ML";' | $(POLYML) --core "bin/$(THE_BIN)" --log "protocol-tests.log"

test check: standard-tests protocol-tests

bin/$(THE_BIN).o: $(QUANTO_HEAP)
	echo 'use "make_bin.ML";' | $(POLYML)

# make the core object without dumping the heap
bin/$(THE_BIN): bin/$(THE_BIN).o
	../tools/polylinkcmd.sh bin/$(THE_BIN) bin/$(THE_BIN).o
	@echo "Success! made binary: bin/$(THE_BIN)"

#	export LD_RUN_PATH="${POLYML_LIB}:${LD_RUN_PATH}" && cc -o bin/$(THE_BIN) bin/$(THE_BIN).o -L${POLYML_LIB} -lpolymain -lpolyml

bin: bin/$(THE_BIN)

echobin:
	@echo export LD_RUN_PATH="${POLYML_LIB}:${LD_RUN_PATH}" && cc -o bin/$(THE_BIN) bin/$(THE_BIN).o -L${POLYML_LIB} -lpolymain -lpolyml

# startup an ML shell using the quanto heap
ml-shell: $(QUANTO_HEAP)
	./bin/polyml-quanto

run: ml-shell

# 
localclean:
	rm -f heaps/*.polyml-heap
	rm -f bin/*.o
	find . -type d -name .polysave | xargs rm -rf
	rm -f bin/$(THE_BIN)

clean: localclean
	$(MAKE) -C $(ISAPLIB_SOURCES) clean

veryclean: clean
vclean: veryclean
