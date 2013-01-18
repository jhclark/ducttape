#!/usr/bin/env bash
set -euo pipefail
scriptDir=$(cd $(dirname $0); pwd)
rootDir=$scriptDir/..

echo >&2 "Building documentation using generator in $rootDir/bin/zinc"
libs=$rootDir/bin/zinc:$(find $rootDir/lib -iname '*.jar' | paste -s -d':')

docdir=tutorial

java -cp $libs ducttape.doc.DucttapeDoc ./tutorial --latex > $docdir/tutorial.tex
java -cp $libs ducttape.doc.DucttapeDoc ./tutorial --markdown > $docdir/TUTORIAL.md
cd $docdir
pdflatex tutorial.tex
pdflatex tutorial.tex # Get the table of contents right

echo >&2 "Tutorial documentation written to: $docdir/tutorial.pdf and $docdir/TUTORIAL.md"
