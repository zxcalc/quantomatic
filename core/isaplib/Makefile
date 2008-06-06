# Targets: 

ML_ISA_SRC_FILES = $(shell ls isa_src/*.ML)
ML_ISAP_SRC_FILES = $(shell ls isap_src/*.ML)
ML_SYSTEM_FILES = $(shell ls ML-Systems/*.ML)

POLYML=poly
POLYML_HEAP=heaps/isalib.polyml-heap

default: polyml

################

# make polyml heap
polyml: $(ML_ISA_SRC_FILES) $(ML_ISAP_SRC_FILES) $(ML_SYSTEM_FILES)
	echo 'use "ML-Systems/polyml.ML"; use "isa_src/ROOT.ML"; use "isap_src/ROOT.ML"; SaveState.saveState "$(POLYML_HEAP)"; quit();' | $(POLYML)
	@echo "Built polyml heap: $(POLYML_HEAP)"

clean: 
	@if test -e $(POLYML_HEAP); then rm -f $(POLYML_HEAP); echo "Removed heaps, now clean."; else echo "No heaps to remove, already clean."; fi