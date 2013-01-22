.PHONY: all quick clean show

XDG_OPEN:=$(shell which xdg-open 2> /dev/null)
MAC_OPEN:=$(shell which open 2> /dev/null)
OPEN:=$(if $(XDG_OPEN),$(XDG_OPEN),$(MAC_OPEN))

OKULAR:=$(strip $(shell qdbus | grep okular))


show: report.pdf
	$(OPEN) report.pdf

update: report.pdf
	qdbus ${OKULAR} /okular/okular__Shell com.trolltech.Qt.QWidget.raise
	qdbus org.kde.konsole /konsole/MainWindow_1 com.trolltech.Qt.QWidget.lower

all: clean show

%.pdf: $(wildcard *.tex) $(wildcard *.bib)
	xelatex $* && bibtex $* && xelatex $* && xelatex $*

quick:
	xelatex report && ${OPEN} report.pdf

clean:
	rm -rf auto *.aux *.bbl *.blg *.log *.out *~ *.pdf

