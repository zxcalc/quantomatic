################################
# dynamic variables checked on call to make. 
################################
ML_SRC_FILES = $(shell ls *.ML)
ISAP_ML_SRC_FILES = $(shell ls ./isaplib/Makefile ./isaplib/*/*.ML ./isaplib/*/*/*.ML)

################################
# variables you might want to set in your own profile setup...
################################
### your local install of PolyML
# POLYML=/home/ldixon/local/polyml-cvs-version
# PATH=$POLYML/bin:$PATH
### add polyml lib to LD_LIBRARY_PATH
# POLYLIB=$POLYML/lib
# LD_LIBRARY_PATH=$POLYLIB:$LD_LIBRARY_PATH
# DYLD_LIBRARY_PATH=$POLYLIB:$DYLD_LIBRARY_PATH

################################
# general variables
################################

# polyml executable
POLYML=poly

################################
# HEAP files defined in make_heap.ML, make_bin.ML
################################
# isaplanner heap file
ISAPLIB_HEAP=../../isaplib/heaps/all.polyml-heap
# quanto heap file
QUANTO_HEAP=heaps/quanto.polyml-heap
# binary file to produce
THE_BIN=quanto-core

################################
# Targets: 
################################
default: bin

# always run make; it's quick and deals with its own deps
$(ISAPLIB_HEAP): 
	cd ../../isaplib; make

# make polyml heap
$(QUANTO_HEAP):  $(ML_SRC_FILES) $(ISAPLIB_HEAP)
	echo 'use "make_heap.ML";' | $(POLYML)
	@echo "Built polyml heap: $(QUANTO_HEAP)"

heap: $(QUANTO_HEAP)

# make the core object without dumping the heap
bin/$(THE_BIN): $(ML_SRC_FILES) $(ISAPLIB_HEAP)
	echo 'use "make_bin.ML";' | $(POLYML)
	cc -o bin/$(THE_BIN) bin/$(THE_BIN).o -L${POLYLIB} -lpolymain -lpolyml
	@echo "Success! made binary: bin/$(THE_BIN)"

bin: bin/$(THE_BIN)

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
