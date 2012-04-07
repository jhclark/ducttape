#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(scala -e 'println(new java.io.File("'$(dirname $0)'").getAbsolutePath)')
libDir=$scriptDir/lib

$scriptDir/get_deps.sh

libs=$libDir/scala-optparse-1.1.jar
libs=$libs:$libDir/scalatest-1.6.1.jar
libs=$libs:$libDir/sqlitejdbc-v056.jar
libs=$libs:$libDir/servlet-api-3.0.jar
libs=$libs:$libDir/jetty-all-8.0.4.v20111024.jar
#libs=$libs:$libDir/scala-io-core_2.9.1-0.3.0.jar
#libs=$libs:$libDir/scala-io-file_2.9.1-0.3.0.jar

echo >&2 "Building source..."
mkdir -p $scriptDir/bin
find $scriptDir/src/main/scala \
  | egrep '\.scala$' \
  | xargs fsc -deprecation -cp $libs \
    -d $scriptDir/bin/ \
  | $scriptDir/color_scalac.awk

echo >&2 "Building JAR..."
(cd $scriptDir/bin; zip -qr $scriptDir/ducttape.jar *)
