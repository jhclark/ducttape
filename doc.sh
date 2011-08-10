#!/usr/bin/env bash
set -euo pipefail

docdir=doc
mkdir -p $docdir
# TODO: JAR path?
scala -cp ducttape.jar DuctTapeDoc ./syntax/tutorial --latex > $docdir/doc.tex
scala -cp ducttape.jar DuctTapeDoc ./syntax/tutorial --markdown > $docdir/doc.md
cd $docdir
pdflatex doc.tex
pdflatex doc.tex # Get the table of contents right

echo >&2 "Tutorial documentation written to: $docdir/doc.pdf and $docdir/doc.md"
