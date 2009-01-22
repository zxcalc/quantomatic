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
THE_BIN='quanto-core'
# polyml executable
POLYML='poly'
 # isaplanner heap file
ISAPLIB_HEAP='./isaplib/heaps/isaplib.polyml-heap'
# quanto heap file
THE_POLY_HEAP='quanto.polyml-heap'

################################
# Targets: 
################################
default: bin

$(ISAPLIB_HEAP): $(ISAP_ML_SRC_FILES)
	@cd ./isaplib; make

# make polyml heap
heaps/$(THE_POLY_HEAP): $(ML_SRC_FILES) $(ISAPLIB_HEAP)
	echo 'PolyML.SaveState.loadState "$(ISAPLIB_HEAP)"; do_and_exit_or_die (fn () => (PolyML.use "ROOT.ML"; PolyML.fullGC (); PolyML.SaveState.saveState "heaps/$(THE_POLY_HEAP)"));' | $(POLYML)
	@echo "Built polyml heap: heaps/$(THE_POLY_HEAP)"

heap: heaps/$(THE_POLY_HEAP)

bin/$(THE_BIN): heaps/$(THE_POLY_HEAP)
	echo 'PolyML.SaveState.loadState "heaps/$(THE_POLY_HEAP)"; do_and_exit_or_die (fn () => (PolyML.export ("bin/$(THE_BIN)", (fn () => (Controller.init())))));' | $(POLYML)
	cc -o bin/$(THE_BIN) bin/$(THE_BIN).o -L${POLYLIB} -lpolymain -lpolyml
	@echo "Success! made binary: bin/$(THE_BIN)"

bin: bin/$(THE_BIN)

# startup an ML shell using the quanto heap
ml-shell: heaps/$(THE_POLY_HEAP)
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
