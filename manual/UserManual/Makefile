# Makefile -- Dylan User Manual
#
# Builds the LaTeX source (main.tex) into Dylan_User_Manual.pdf.
# Requires: pdflatex and makeindex (TeX Live).

NAME    := Dylan_User_Manual
MAIN    := main
LATEX   := pdflatex
LFLAGS  := -interaction=nonstopmode -halt-on-error -jobname=$(NAME)
MAKEIDX := makeindex

SOURCES := $(MAIN).tex $(wildcard chapters/*.tex)

.PHONY: all clean cleanall

all: $(NAME).pdf

# Full build: pdflatex -> makeindex -> pdflatex x2
# (two final passes resolve the table of contents, index, and page references).
$(NAME).pdf: $(SOURCES)
	$(LATEX) $(LFLAGS) $(MAIN).tex
	$(MAKEIDX) $(NAME).idx
	$(LATEX) $(LFLAGS) $(MAIN).tex
	$(LATEX) $(LFLAGS) $(MAIN).tex

# Remove intermediate build files (keeps the PDF).
clean:
	rm -f $(NAME).aux $(NAME).log $(NAME).toc $(NAME).out \
	      $(NAME).idx $(NAME).ind $(NAME).ilg chapters/*.aux

# Remove intermediate build files and the generated PDF.
cleanall: clean
	rm -f $(NAME).pdf
