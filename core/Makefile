# Targets: 

ML_SRC_FILES = $(shell ls *.ML)

QUANTO_DIR=$(HOME)/.quantomatic
QUANTO_BIN=quantomatic
POLYML=poly
# POLYML_LIB=/home/ldixon/local/polyml-5.2.0/lib
ISAPLIB_HEAP=./isaplib/heaps/isalib.polyml-heap
QUANTO_HEAP=$(QUANTO_DIR)/quanto.polyml-heap

default: bin

################

$(ISAPLIB_HEAP):
	@cd ./isaplib; make polyml

# make polyml heap
$(QUANTO_HEAP): $(ML_SRC_FILES) $(ISAPLIB_HEAP)
	@if test -e $(QUANTO_DIR); then echo "Quantomatic dir: $(QUANTO_DIR) exists"; else mkdir $(QUANTO_DIR); echo "Quantomatic dir made: $(QUANTO_DIR)"; fi
	echo 'PolyML.SaveState.loadState "$(ISAPLIB_HEAP)"; use "ROOT.ML"; PolyML.SaveState.saveState "$(QUANTO_HEAP)"; quit();' | $(POLYML)
	@echo "Built polyml heap: $(QUANTO_HEAP)"

heap: $(QUANTO_HEAP)

bin/$(QUANTO_BIN): $(ML_SRC_FILES) $(ISAPLIB_HEAP)
	echo 'PolyML.SaveState.loadState "$(ISAPLIB_HEAP)"; use "ROOT.ML"; PolyML.export ("bin/quantomatic", (fn () => (Controller.init(); ()))); quit();' | $(POLYML)
#	cc -o bin/$(QUANTO_BIN) bin/$(QUANTO_BIN).o -lpolymain -lpolyml
	cc -o bin/$(QUANTO_BIN) bin/$(QUANTO_BIN).o -L${POLYLIB} -lpolymain -lpolyml
	@echo "made quanto binary: $(QUANTO_BIN)"

bin: bin/$(QUANTO_BIN)

#install: polyml-bin
#	@if test -e $(QUANTO_DIR); then echo "Quantomatic dir: $(QUANTO_DIR) exists"; else mkdir $(QUANTO_DIR); echo "Quantomatic dir made: $(QUANTO_DIR)"; fi
#	cp bin/$(QUANTO_BIN) $(QUANTO_DIR)/
#	cp bin/run-$(QUANTO_BIN) $(QUANTO_DIR)/

# startup an ML shell using the quanto heap
quanto-ml-shell: $(QUANTO_HEAP)
	echo 'PolyML.SaveState.loadState "$(QUANTO_HEAP)";' > bin/loadstate.ML
	cat bin/loadstate.ML - | poly

#clean_heap_%: 
#	@if test -e %; then echo "rm -f %"; echo "Removed heap: %"; else echo "No heap % to be remove, already clean."; fi

# clean: clean_heap_$(QUANTO_HEAP) clean_heap_$(ISAPLIB_HEAP)
clean:
	rm -f bin/*.o
	rm -f bin/quantomatic
	cd isaplib; make clean