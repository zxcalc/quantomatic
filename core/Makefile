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
# binary file to produce
THE_BIN=quanto-core
# polyml executable
POLYML=poly
 # isaplanner heap file
ISAPLIB_HEAP=isaplib/heaps/all.polyml-heap
# quanto heap file
QUANTO_HEAP=heaps/quanto.polyml-heap

################################
# Targets: 
################################
default: bin

# always run make; it's quick and deals with its own deps
$(ISAPLIB_HEAP):
	cd ./isaplib; make

# make polyml heap
$(QUANTO_HEAP):  $(ML_SRC_FILES) $(ISAPLIB_HEAP)
	echo 'PolyML.SaveState.loadState "$(ISAPLIB_HEAP)"; use "make_heap.ML"; PolyML.SaveState.saveState "$(QUANTO_HEAP)";' | $(POLYML)
	@echo "Built polyml heap: $(QUANTO_HEAP)"

heap: $(QUANTO_HEAP)


# make the core object without dumping the heap
noheap: $(ML_SRC_FILES) $(ISAPLIB_HEAP)
	echo 'PolyML.SaveState.loadState "$(ISAPLIB_HEAP)"; use "make_heap.ML"; do_and_exit_or_die (fn () => (forget_all(); PolyML.shareCommonData run; PolyML.fullGC (); PolyML.export ("bin/$(THE_BIN)", (fn () => do_and_exit_or_die run))));' | $(POLYML)
	cc -o bin/$(THE_BIN) bin/$(THE_BIN).o -L${POLYLIB} -lpolymain -lpolyml
	@echo "Success! made binary: bin/$(THE_BIN)"
	touch noheap # token file to indicate we are up to date

bin/$(THE_BIN): $(QUANTO_HEAP)
	echo 'PolyML.SaveState.loadState "$(QUANTO_HEAP)"; do_and_exit_or_die (fn () => (forget_all(); PolyML.shareCommonData run; PolyML.fullGC (); PolyML.export ("bin/$(THE_BIN)", (fn () => do_and_exit_or_die run))));' | $(POLYML)
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
	rm -f bin/$(THE_BIN)

veryclean: clean
	cd isaplib; make clean

vclean: veryclean
