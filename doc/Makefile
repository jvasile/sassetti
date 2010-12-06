PANDOC=`which pandoc`
PDFLATEX=`which pdflatex`

# List text files in the order in which you want them to appear in the
# complete manual:
SOURCES=README.txt INSTALL.txt hacking.txt COPYING.txt colophon.txt

HTML=sassetti.html $(patsubst %.txt,%.html,$(wildcard *.txt))
LATEX=sassetti.tex $(patsubst %.txt,%.tex,$(wildcard *.txt))
PDF=sassetti.pdf $(patsubst %.txt,%.pdf,$(wildcard *.txt))

all: pdf html

###############################################################################
$(SOURCES):
	ln -s ../`echo $@ | sed -e 's/.txt$$//'` $@
###############################################################################
%.tex: %.txt Makefile
	$(PANDOC) -s --toc -f markdown -t latex --standalone -o $@ $<

sassetti.tex: $(SOURCES) Makefile
	$(PANDOC) -s -o sassetti.tex -f markdown $(SOURCES)

latex: $(LATEX)
###############################################################################
%.html: %.txt Makefile header.html footer.html style.css
	$(PANDOC) -s --toc -c style.css -f markdown -t html --standalone -o $@ header.html $< footer.html

sassetti.html: $(SOURCES) Makefile
	$(PANDOC) -s --toc -o sassetti.html -f markdown $(SOURCES)

html: $(HTML)
###############################################################################
%.pdf: %.tex Makefile
	$(PDFLATEX) $<

pdf: $(PDF)
###############################################################################

clean-latex:
	rm -rf *.log *.out *.aux

clean: clean-latex
	rm -rf $(PDF) $(LATEX) $(HTML)

sterile: clean
	rm -rf $(SOURCES)
