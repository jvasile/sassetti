PANDOC=@`which pandoc`
PDFLATEX=@`which pdflatex`

# List text files in the order in which you want them to appear in the
# complete manual:
SOURCES=README.txt INSTALL.txt hacking.txt COPYING.txt colophon.txt

HTML=sassetti.html $(patsubst %.txt,%.html,$(SOURCES))
LATEX=sassetti.tex $(patsubst %.txt,%.tex,$(SOURCES))
PDF=sassetti.pdf $(patsubst %.txt,%.pdf,$(SOURCES))

all: pdf html

###############################################################################
$(SOURCES): 
	ln -s ../`echo $@ | sed -e 's/.txt$$//'` $@
###############################################################################
%.tex: %.txt Makefile
	$(PANDOC) -s --toc -f markdown --standalone -o $@ $<

sassetti.tex: $(SOURCES) Makefile
	$(PANDOC) -s --toc -f markdown -o sassetti.tex $(SOURCES)

latex: $(LATEX)
###############################################################################
%.html: %.txt Makefile header.html footer.html style.css
	$(PANDOC) -s --toc -c style.css -f markdown --standalone -o $@ header.html $< footer.html

sassetti.html: $(SOURCES) Makefile
	$(PANDOC) -s --toc -o sassetti.html -f markdown $(SOURCES)

html: $(HTML)
###############################################################################
%.pdf: %.tex Makefile
	$(PDFLATEX) $<
	$(PDFLATEX) $<  # yes, do it twice so the toc works

pdf: $(PDF)
###############################################################################

clean-latex:
	rm -rf *.log *.out *.aux *.toc

clean: clean-latex
	rm -rf $(PDF) $(LATEX) $(HTML)
	rm -rf README.txt INSTALL.txt COPYING.txt
