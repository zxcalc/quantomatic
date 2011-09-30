################################
# dynamic variables checked on call to make. 
################################
ML_SRC_FILES = $(shell ls *.ML)
ISAP_ML_SRC_FILES = $(shell ls ../../isaplib/Makefile ../../isaplib/*/*.ML ../../isaplib/*/*/*.ML)

################################
# variables you might want to set in your own profile setup...
################################
### your local install of PolyML
# POLYML_HOME=/home/ldixon/local/polyml-cvs-version

################################
# general variables
################################

# polyml executable
POLYML_HOME=$(shell ../tools/findpoly.sh)
POLYML=$(POLYML_HOME)/bin/poly
POLYML_LIB=$(POLYML_HOME)/lib

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

heaps/:
	mkdir heaps

# make polyml heap
$(QUANTO_HEAP): heaps/ $(ML_SRC_FILES)
	echo 'use "make_heap.ML";' | $(POLYML)
	@echo "Built polyml heap: $(QUANTO_HEAP)"

heap: $(QUANTO_HEAP)

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
clean:
	rm -f heaps/*.polyml-heap
	rm -f bin/*.o
	rm -rf .polysave
	rm -f bin/$(THE_BIN)

veryclean: clean
	cd isaplib; make clean

vclean: veryclean
