# build the quantomatic heap (for development setup)

THEORY_FILES=lib.thy.ML core.thy.ML theories.thy.ML controller.thy.ML

heap: theory_files
	cat build_heap.ML | ML_SYSTEM=polyml ML_PLATFORM=unknown poly

theory_files : $(THEORY_FILES)

%.thy.ML : %.thy
	sed -n 's/ML_file "\(.*\)"/use "\1";/p' $< > $@