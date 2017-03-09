TEX_FILES:=$(wildcard *.tex)
all: main

main: ${TEX_FILES} Makefile
	pdflatex $@
	bibtex $@
	pdflatex $@
	pdflatex $@

clean:
	rm -f *.aux *.bbl *.blg *.log *.dvi *.out *.idx *.bak *~ *.toc *.lof
	rm -f main.pdf
