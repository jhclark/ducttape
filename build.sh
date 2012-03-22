#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(scala -e 'println(new java.io.File("'$(dirname $0)'").getAbsolutePath)')
libDir=$scriptDir/lib

$scriptDir/get_deps.sh

libs=""
libs="$libs:$libDir/sqlitejdbc-v056.jar"
libs="$libs:$libDir/scala-optparse-1.1.jar"
libs="$libs:$libDir/test/scalatest-1.7.1.jar"
libs="$libs:$libDir/webui/servlet-api-3.0.jar"
libs="$libs:$libDir/webui/jetty-all-8.0.4.v20111024.jar"

echo >&2 "Building source..."
mkdir -p $scriptDir/bin
find $scriptDir/src/main/scala \
  | egrep '\.scala$' \
  | xargs fsc -cp $libs  \
    -d $scriptDir/bin/ \
  | $scriptDir/color_scalac.awk

echo >&2 "Building JAR..."
(cd $scriptDir/bin; zip -qr $scriptDir/ducttape.jar *)
