DOCDIR=../dist/doc

PANDOC=pandoc
PDFLATEX=pdflatex


# List text files in the order in which you want them to appear in the
# complete manual:
SOURCES=README.txt INSTALL.txt hacking.txt COPYING.txt colophon.txt
MAN_SOURCES=$(patsubst COPYING.txt,copyright_notice00,$(SOURCES))

HTML=sassetti.html $(patsubst %.txt,%.html,$(SOURCES))
LATEX=sassetti.tex $(patsubst %.txt,%.tex,$(SOURCES))
PDF=sassetti.pdf $(patsubst %.txt,%.pdf,$(SOURCES))
MAN=sassetti.1
OUTPUTS=$(PDF) $(LATEX) $(HTML) $(MAN)

all: $(OUTPUTS)

dist: all
	@mkdir -p $(DOCDIR)
	cp $(OUTPUTS) $(DOCDIR)
###############################################################################
$(SOURCES):
	@ln -s ../$(patsubst %.txt,%,$@) $@
###############################################################################
$(MAN): $(SOURCES)
	@csplit -s -f copyright_notice COPYING.txt '/##/'
	$(PANDOC) -s -t man -o $@ $(MAN_SOURCES)
	@rm copyright_notice0?
manpages: $(MAN)
###############################################################################
%.tex: %.txt
	$(PANDOC) -s --toc -f markdown --standalone -o $@ $<

sassetti.tex: $(SOURCES)
	$(PANDOC) -s --toc -f markdown -o sassetti.tex $(SOURCES)

latex: $(LATEX)
###############################################################################
%.html: %.txt header.html footer.html style.css
	$(PANDOC) -s --toc -c style.css -f markdown --standalone -o $@ header.html $< footer.html

sassetti.html: $(SOURCES)
	$(PANDOC) -s --toc -o sassetti.html -f markdown $(SOURCES)

html: $(HTML)
###############################################################################
%.pdf: %.tex
	$(PDFLATEX) -interaction=batchmode $< >/dev/null
	$(PDFLATEX) -interaction=batchmode $< >/dev/null  # yes, do it twice so the toc works

pdf: $(PDF)
###############################################################################

clean-latex:
	rm -rf *.log *.out *.aux *.toc

clean: clean-latex
	rm -rf $(OUTPUTS)
	rm -f README.txt INSTALL.txt COPYING.txt
	rm -f copyright_notice0?
	rm -rf \#*\#
