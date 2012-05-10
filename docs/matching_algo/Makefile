
default:
	mkdir -p build
	cp matching_algo.tex concrete_part.dot build/
	(cd build; \
	dot -Tpdf concrete_part.dot -o concrete_part.pdf; \
	pdflatex matching_algo.tex)
	
clean:
	rm -rf build