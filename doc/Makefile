DOCDIR=../dist/doc

PANDOC=pandoc
PDFLATEX=pdflatex


# List text files in the order in which you want them to appear in the
# complete manual:
SOURCES=README.txt INSTALL.txt hacking.txt TODO.txt COPYING.txt colophon.txt
MAN_SOURCES=$(patsubst COPYING.txt,copyright_notice00,$(SOURCES))

HTML=sassetti.html $(patsubst %.txt,%.html,$(SOURCES))
LATEX=sassetti.tex $(patsubst %.txt,%.tex,$(SOURCES))
PDF=sassetti.pdf $(patsubst %.txt,%.pdf,$(SOURCES))
MAN=sassetti.1

OUTPUTS=$(PDF) $(LATEX) $(HTML) $(MAN)
DIST_OUTPUT=$(patsubst %,$(DOCDIR)/%,$(OUTPUTS))

all: $(OUTPUTS)

$(DOCDIR)/%: %
	cp $< $@
dist: $(DIST_OUTPUT)
###############################################################################
$(SOURCES):
	@rm -f $@
	@ln -s ../$(patsubst %.txt,%,$@) $@
../TODO : $(SOURCES) ../*.lisp ../*.asd
	grep -ro --exclude=.git* --exclude=sassetti.1 --exclude=*.tex --exclude=*.html \
	     --exclude=README.txt --exclude=INSTALL.txt \
	     --exclude=TODO.txt --exclude=COPYING.txt \
	     "TODO\:.*" ../* 2>/dev/null | \
	     sed -e "s/TODO\://g" | \
	     sed -e "s/^..\//* /g" \
	     > ../TODO
###############################################################################
$(MAN): $(SOURCES) ../TODO
	@csplit -s -f copyright_notice COPYING.txt '/##/'
	$(PANDOC) -s -t man -o $@ $(MAN_SOURCES)
	@rm copyright_notice0?
manpages: $(MAN)
###############################################################################
%.tex: %.txt
	$(PANDOC) -s --toc -f markdown --standalone -o $@ $<

hacking.tex: hacking.txt ../TODO
	$(PANDOC) -s --toc -f markdown -o $@ hacking.txt ../TODO

sassetti.tex: $(SOURCES) ../TODO
	$(PANDOC) -s --toc -f markdown -o $@ $(SOURCES)

latex: $(LATEX)
###############################################################################
%.html: %.txt header.html footer.html style.css
	$(PANDOC) -s --toc -c style.css -f markdown --standalone -o $@ header.html $< footer.html

hacking.html: hacking.txt ../TODO
	$(PANDOC) -s --toc -o $@ -f markdown hacking.txt ../TODO

sassetti.html: $(SOURCES) ../TODO
	$(PANDOC) -s --toc -o $@ -f markdown $(SOURCES)

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
	rm -f $(OUTPUTS) README.txt INSTALL.txt TODO.txt COPYING.txt copyright_notice0? \#*\# ../TODO
