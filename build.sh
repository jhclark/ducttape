#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(scala -e 'println(new java.io.File("'$(dirname $0)'").getAbsolutePath)')
libDir=$scriptDir/lib

$scriptDir/get_deps.sh

echo >&2 "Building source..."
mkdir -p $scriptDir/bin
find $scriptDir/src/main/scala \
  | egrep '\.scala$' \
  | xargs fsc -cp $libDir/scalatest-1.6.1.jar:$libDir/sqlitejdbc-v056.jar:$libDir/servlet-api-3.0.jar:$libDir/jetty-all-8.0.4.v20111024.jar  \
    -d $scriptDir/bin/ \
  | $scriptDir/color_scalac.awk

echo >&2 "Building JAR..."
(cd $scriptDir/bin; zip -qr $scriptDir/ducttape.jar *)
