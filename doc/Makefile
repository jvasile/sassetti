DOCDIR=../dist/doc

PANDOC=pandoc
PDFLATEX=pdflatex


# List text files in the order in which you want them to appear in the
# complete manual:
SOURCES=README.mdwn INSTALL.mdwn hacking.mdwn TODO.mdwn COPYING.mdwn colophon.mdwn
MAN_SOURCES=$(patsubst COPYING.mdwn,copyright_notice00,$(SOURCES))

HTML=sassetti.html $(patsubst %.mdwn,%.html,$(SOURCES))
LATEX=sassetti.tex $(patsubst %.mdwn,%.tex,$(SOURCES))
PDF=sassetti.pdf $(patsubst %.mdwn,%.pdf,$(SOURCES))
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
	@ln -s ../$(patsubst %.mdwn,%,$@) $@
../TODO : $(SOURCES) ../*.lisp ../*.asd
	grep -ro --exclude=.git* --exclude=sassetti.1 --exclude=*.tex --exclude=*.html \
	     --exclude=README.mdwn --exclude=INSTALL.mdwn \
	     --exclude=TODO.mdwn --exclude=COPYING.mdwn \
	     "TODO\:.*" ../* 2>/dev/null | \
	     sed -e "s/TODO\://g" | \
	     sed -e "s/^..\//* /g" \
	     > ../TODO
###############################################################################
$(MAN): $(SOURCES) ../TODO
	@csplit -s -f copyright_notice COPYING.mdwn '/##/'
	$(PANDOC) -s -t man -o $@ $(MAN_SOURCES)
	@rm copyright_notice0?
manpages: $(MAN)
###############################################################################
%.tex: %.mdwn
	$(PANDOC) -s --toc -f markdown --standalone -o $@ $<

hacking.tex: hacking.mdwn ../TODO
	$(PANDOC) -s --toc -f markdown -o $@ hacking.mdwn ../TODO

sassetti.tex: $(SOURCES) ../TODO
	$(PANDOC) -s --toc -f markdown -o $@ $(SOURCES)

latex: $(LATEX)
###############################################################################
%.html: %.mdwn header.html footer.html style.css
	$(PANDOC) -s --toc -c style.css -f markdown --standalone -o $@ header.html $< footer.html

hacking.html: hacking.mdwn ../TODO
	$(PANDOC) -s --toc -o $@ -f markdown hacking.mdwn ../TODO

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
	rm -f *.log *.out *.aux *.toc

clean: clean-latex
	rm -f $(OUTPUTS) README.mdwn INSTALL.mdwn TODO.mdwn COPYING.mdwn copyright_notice0? \#*\# ../TODO
