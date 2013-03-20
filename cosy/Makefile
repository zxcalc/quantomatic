POLYML_HOME=$(shell ../tools/findpoly.sh)
POLYML=$(POLYML_HOME)/bin/poly -H 200
POLYML_LIB=$(POLYML_HOME)/lib

polychrome.:
	echo 'use "make_polychrome.ML";' | $(POLYML)
#polychrome.:
#  echo 'use "make_polychrome.ML";' | poly -H 200
	
clean:
	rm -f chromeui/polychrome.polyml-heap
	find . -type d -name .polysave | xargs rm -rf
