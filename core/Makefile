# Targets: 

ML_SRC_FILES = $(shell ls *.ML)

THE_BIN=quantomatic
POLYML=poly
# POLYML_LIB=/home/ldixon/local/polyml-5.2.0/lib
ISAPLIB_HEAP=./isaplib/heaps/isaplib.polyml-heap
THE_POLY_HEAP=quanto.polyml-heap

default: bin

################

$(ISAPLIB_HEAP):
	@cd ./isaplib; make

# make polyml heap
heaps/$(THE_POLY_HEAP): $(ML_SRC_FILES) $(ISAPLIB_HEAP)
	echo 'PolyML.SaveState.loadState "$(ISAPLIB_HEAP)"; use "ROOT.ML"; PolyML.SaveState.saveState "heaps/$(THE_POLY_HEAP)"; quit();' | $(POLYML)
	@echo "Built polyml heap: heaps/$(THE_POLY_HEAP)"

heap: heaps/$(THE_POLY_HEAP)

bin/$(THE_BIN): heaps/$(THE_POLY_HEAP)
	echo 'PolyML.SaveState.loadState "heaps/$(THE_POLY_HEAP)"; PolyML.export ("bin/$(THE_BIN)", (fn () => (Controller.init(); ()))); quit();' | $(POLYML)
	cc -o bin/$(THE_BIN) bin/$(THE_BIN).o -L${POLYLIB} -lpolymain -lpolyml
	@echo "Success! made binary: $(THE_BIN)"

bin: bin/$(THE_BIN)

# startup an ML shell using the quanto heap
ml-shell: heaps/$(THE_POLY_HEAP)
	echo 'PolyML.SaveState.loadState "heaps/$(THE_POLY_HEAP)";' > heaps/run-$(THE_POLY_HEAP).ML
	cat heaps/run-$(THE_POLY_HEAP).ML - | poly

run: ml-shell

# 
clean:
	rm -f heaps/*.ML heaps/*.polyml-heap
	rm -f bin/*.o
	rm -f bin/$(THE_BIN)
	cd isaplib; make clean