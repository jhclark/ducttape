#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(scala -e 'println(new java.io.File("'$(dirname $0)'").getAbsolutePath)')
libDir=$scriptDir/lib

$scriptDir/get_deps.sh

libs=""
libs="$libs:$libDir/sqlitejdbc-v056.jar"
libs="$libs:$libDir/scala-optparse-1.1.jar"
libs="$libs:$libDir/commons-lang3-3.1.jar"
libs="$libs:$libDir/scalatest-1.6.1.jar"
libs="$libs:$libDir/webui/servlet-api-3.0.jar"
libs="$libs:$libDir/webui/jetty-all-8.0.4.v20111024.jar"
libs="$libs:$libDir/junit-4.10.jar"

echo >&2 "Building source..."
mkdir -p $scriptDir/bin
find $scriptDir/src/main/scala $scriptDir/src/test/scala \
  | egrep '\.scala$' \
  | xargs fsc -deprecation -cp $libs  \
    -d $scriptDir/bin/ \
  | $scriptDir/color_scalac.awk

echo >&2 "Building JAR..."
(cd $scriptDir/bin; zip -qr $scriptDir/ducttape.jar *)
