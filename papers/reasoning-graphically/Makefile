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

SRC=Makefile $(BASENAME).tex $(BIBFILE) $(EPS_FILES) $(STY_FILES) $(CLS_FILES)


################
gvify = if test -e .gvlock; \
	then\
		echo "gv is running, it should now update itself. If not, remove .gvlock";\
	else\
		echo "gv locked" > .gvlock; gv $(BASENAME).ps.gz ; rm .gvlock;\
	fi &

################

dvi : $(BASENAME).dvi
pdf : $(BASENAME).pdf
ps : $(BASENAME).ps.gz

preview: $(BASENAME).dvi
	@echo "Previewing  with xdvi..."
	@xdvi -hush -nopost -s 10 $(BASENAME) 2> /dev/null &

view: $(BASENAME).ps.gz
	@echo "Viewing with ghostview..."
	@$(gvify)

all : $(BASENAME).ps
	@echo "making ps.gz and pdf"
	@ps2pdf $(BASENAME).ps $(BASENAME).pdf
	@gzip -f $(BASENAME).ps

################

$(BASENAME).ps: $(BASENAME).dvi
	@echo "Generating PostScript..."
	@dvips -P pdf -o $(BASENAME).ps $(BASENAME).dvi

$(BASENAME).aux: $(SRC)
	@echo "Compiling LaTeX source to get aux file..."
	@export TEXINPUTS=$(TEXINPUTS):./:./tex && latex $(BASENAME)  

$(BASENAME).bbl: $(BASENAME).aux $(BIBFILE)
	@echo "Compiling bibtex to get bbl file..."
	@-bibtex $(BASENAME) # "-" is to not fail make on errros. 

$(BASENAME).dvi: $(SRC) $(BASENAME).bbl
	@echo "Compiling LaTeX source with bbl to make final DVI..."
	@export TEXINPUTS=$(TEXINPUTS):./:./tex && latex $(BASENAME)
	@export TEXINPUTS=$(TEXINPUTS):./:./tex && latex $(BASENAME)

$(BASENAME).pdf : $(BASENAME).ps
	@ps2pdf $(BASENAME).ps $(BASENAME).pdf
	@rm $(BASENAME).ps

$(BASENAME).ps.gz: $(BASENAME).ps
	@echo "gzipping..."
	@gzip -f $(BASENAME).ps

## 
clean:
	@echo "Removing DVI, and temporary files..."
	@rm -f $(BASENAME).aux $(BASENAME).log $(BASENAME)*~ 
	@rm -f $(BASENAME).bbl $(BASENAME).blg
	@rm -f .gvlock

veryclean: clean
	@echo "Removing ps, ps.gz, pdf files..."
	@rm -f $(BASENAME).dvi $(BASENAME).ps $(BASENAME).ps.gz $(BASENAME).pdf


# EOF: Makefile
