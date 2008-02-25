# Targets: 
#
#   make [preview]    - compile LaTeX, convert to PS, display with xdvi
#   make view         - compile LaTeX, convert to PS, display with ghostview
#   make dvi          - compile LaTeX, convert to DVI
#   make ps           - compile LaTeX, convert to PS.GZ
#   make pdf          - compile LaTeX, convert to PDF
#   make all          - make both pdf and ps.gz files. 
#   make clean        - delete all temporary / backup files


# BASENAME is the name of the tex file (without the postfix .tex)
BASENAME=paper
# BIBFILE is the filename that holds the bibtex entries
BIBFILE=bibfile.bib

default: view			# alternatives = view, preview, ps, pdf, dvi

STY_FILES = $(shell ls tex/*.sty)
CLS_FILES = $(shell ls tex/*.cls)
EPS_FILES = $(shell ls images/*.eps)
#EPS_FILES != `ls images/*.eps`
#PDF_OF_EPS_FILES := ${EPS_FILES:s/.eps/.pdf/}
PDF_OF_EPS_FILES = $(shell ls images/*.eps | sed s/eps$$/pdf/)

BASIC_SRC=Makefile $(BASENAME).tex $(BIBFILE) $(STY_FILES) $(CLS_FILES)
PS_SRC=$(BASIC_SRC) $(EPS_FILES)
PDF_SRC=$(BASIC_SRC) $(PDF_OF_EPS_FILES)

# if mac (has osascript command) then preview pdf,
# if Linux and has gv then preview ps.gz
PREVIEW_KIND_DEP := $(shell if test -x /usr/bin/osascript; then echo "$(BASENAME).pdf"; elif test -x /usr/bin/gv; then echo "$(BASENAME).ps.gz"; else echo ""; fi)

################
gvify = if test -e .gvlock; \
	then\
		echo "gv is running, it should now update itself. If not, remove .gvlock";\
	else\
		echo "gv locked, created a .gvlock file." > .gvlock; gv $(BASENAME).ps.gz ; rm .gvlock;\
	fi;
openify = open $(BASENAME).pdf;
# if mac (has osascript command) then preview pdf,
# if Linux and has gv then preview ps.gz
dopreview = if test -x /usr/bin/osascript; \
	then $(openify)\
  elif test -x /usr/bin/gv; \
	then $(gvify)\
	else \
	  echo "Don't know how to preview the document: install gv or use a mac.";\
	fi&

################

dvi : $(BASENAME).dvi
pdf : $(BASENAME).pdf
ps : $(BASENAME).ps.gz

# debugging, echo some macro vars
echopaths:
	@echo "PREVIEW_KIND_DEP: $(PREVIEW_KIND_DEP)"
	@echo "EPS_FILES: $(EPS_FILES)"
	@echo "PDF_OF_EPS_FILES: $(PDF_OF_EPS_FILES)"

# make pdf images from eps ones
images/%.pdf : images/%.eps 
	@epstopdf $<

preview: $(BASENAME).dvi
	@echo "Previewing  with xdvi..."
	@xdvi -hush -nopost -s 10 $(BASENAME) 2> /dev/null &

view: $(PREVIEW_KIND_DEP)
	@echo "Trying to viewing with gv or mac open..."
	@$(dopreview)

all : $(BASENAME).ps $(BASENAME).pdf
	@echo "made ps.gz and pdf"
#	@ps2pdf $(BASENAME).ps $(BASENAME).pdf
#	@gzip -f $(BASENAME).ps

################

$(BASENAME).ps: $(BASENAME).dvi
	@echo "Generating PostScript..."
	@dvips -P pdf -o $(BASENAME).ps $(BASENAME).dvi

$(BASENAME).aux: $(PS_SRC)
	@echo "Compiling LaTeX source to get aux file..."
	@export TEXINPUTS=$(TEXINPUTS):./:./tex && latex $(BASENAME)  

$(BASENAME).bbl: $(BASENAME).aux $(BIBFILE)
	@echo "Compiling bibtex to get bbl file..."
	@-bibtex $(BASENAME) # "-" is to not fail make on errros. 

$(BASENAME).dvi: $(PS_SRC) $(BASENAME).bbl
	@echo "Compiling LaTeX source using bbl to make final DVI..."
	@export TEXINPUTS=$(TEXINPUTS):./:./tex && latex $(BASENAME)
	@export TEXINPUTS=$(TEXINPUTS):./:./tex && latex $(BASENAME)

$(BASENAME).pdf : $(PDF_SRC) $(BASENAME).bbl
	@echo "Compiling PDF LaTeX source using bbl to make final DVI..."
	@export TEXINPUTS=$(TEXINPUTS):./:./tex && pdflatex $(BASENAME)
	@export TEXINPUTS=$(TEXINPUTS):./:./tex && pdflatex $(BASENAME)

# old version, made from ps
#$(BASENAME).pdf : $(BASENAME).ps
#	@ps2pdf $(BASENAME).ps $(BASENAME).pdf
#	@rm $(BASENAME).ps

$(BASENAME).ps.gz: $(BASENAME).ps
	@echo "gzipping..."
	@gzip -f $(BASENAME).ps

## 
clean:
	@echo "Removing DVI, and temporary files..."
	@rm -f $(BASENAME).aux $(BASENAME).log $(BASENAME)*~ 
	@rm -f $(PDF_OF_EPS_FILES)
	@rm -f $(BASENAME).bbl $(BASENAME).blg
	@rm -f .gvlock

veryclean: clean
	@echo "Removing ps, ps.gz, pdf files..."
	@rm -f $(BASENAME).dvi $(BASENAME).ps $(BASENAME).ps.gz $(BASENAME).pdf
	@rm -f ./*~


# EOF: Makefile
