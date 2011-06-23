#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(scala -e 'println(new java.io.File("'$(dirname $0)'").getAbsolutePath)')

echo >&2 "Building source..."
mkdir -p $scriptDir/bin
fsc -cp $scriptDir/lib/scalatest-1.4.1.jar \
    -d $scriptDir/bin/ \
    $scriptDir/scala/*.scala

echo >&2 "Building JAR..."
(cd $scriptDir/bin; zip -qr $scriptDir/ducttape.jar *)