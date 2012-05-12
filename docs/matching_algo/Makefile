
default:
	mkdir -p build
	cp matching_algo.tex *.dot build/
	(cd build; \
	dot -Tpdf concrete_part.dot -o concrete_part.pdf; \
	dot -Tpdf pattern_part.dot -o pattern_part.pdf; \
	pdflatex matching_algo.tex)
	
clean:
	rm -rf build