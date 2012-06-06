#!/usr/bin/env bash
set -euo pipefail
scriptDir=$(cd $(dirname $0); pwd)
rootDir=$scriptDir/..

libs=$rootDir/ducttape.jar
#libs=$rootDir/bin
libs=$libs:$rootDir/lib/scala/scala-library-2.9.2.jar # DEV-ONLY

libs=$libs:$rootDir/lib/scala-optparse-1.1.jar # DEV-ONLY
libs=$libs:$rootDir/lib/commons-lang3-3.1.jar # DEV-ONLY
libs=$libs:$rootDir/lib/commons-io-2.2.jar # DEV-ONLY
libs=$libs:$rootDir/lib/grizzled-slf4j_2.9.1-1-0.6.8.jar # DEV-ONLY

# Use slf4j's java.util logger (at runtime only) # DEV-ONLY
libs=$libs:$rootDir/lib/slf4j-api-1.6.4.jar # DEV-ONLY
libs=$libs:$rootDir/lib/slf4j-jdk14-1.6.4.jar # DEV-ONLY


docdir=tutorial
#mkdir -p $docdir

java -cp $libs ducttape.doc.DucttapeDoc ./tutorial --latex > $docdir/tutorial.tex
java -cp $libs ducttape.doc.DucttapeDoc ./tutorial --markdown > $docdir/TUTORIAL.md
cd $docdir
pdflatex tutorial.tex
pdflatex tutorial.tex # Get the table of contents right

echo >&2 "Tutorial documentation written to: $docdir/tutorial.pdf and $docdir/TUTORIAL.md"
