
default: build/matching_algo.pdf

build/:
	mkdir -p build

build/%.pdf: build/ %.dot
	cp $*.dot build/
	cd build; dot -Tpdf $*.dot -o $*.pdf

build/%.pdf: build/ build/concrete_part.pdf build/concrete_wrapper.pdf build/pattern_part.pdf build/nonstrict.pdf %.tex
	cp $*.tex build/
	cd build; pdflatex $*.tex

view: build/matching_algo.pdf
	xdg-open build/matching_algo.pdf
	
clean:
	rm -rf build

.PHONY: default view clean
