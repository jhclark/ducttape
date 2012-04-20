#!/usr/bin/env bash
set -euo pipefail
scriptDir=$(dirname $0)

libs=$scriptDir/ducttape.jar
libs=$libs:$scriptDir/lib/scala-library.jar

docdir=doc
mkdir -p $docdir
# TODO: JAR path?
java -cp $libs ducttape.doc.DuctTapeDoc ./syntax/tutorial --latex > $docdir/doc.tex
java -cp $libs ducttape.doc.DuctTapeDoc ./syntax/tutorial --markdown > $docdir/doc.md
cd $docdir
pdflatex doc.tex
pdflatex doc.tex # Get the table of contents right

echo >&2 "Tutorial documentation written to: $docdir/doc.pdf and $docdir/doc.md"
