#!/usr/bin/env bash
set -euo pipefail

docdir=doc
mkdir -p $docdir
# TODO: Remove hardcoded version
scala -cp target/scala-2.9.0.final/classes/ DuctTapeDoc ./syntax/tutorial > $docdir/doc.tex
cd $docdir
pdflatex doc.tex
pdflatex doc.tex # Get the table of contents right

echo >&2 "Tutorial documentation written to: $docdir/doc.pdf"