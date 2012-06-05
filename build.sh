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
libs="$libs:$libDir/commons-io-2.2.jar"
libs="$libs:$libDir/grizzled-slf4j_2.9.1-1-0.6.8.jar"

echo >&2 "Building source..."
mkdir -p $scriptDir/bin
if [ "$1" == "no-test" ] ; then
    dirs=$scriptDir/src/main/scala
else
    dirs=$scriptDir/src/main/scala $scriptDir/src/test/scala
fi
find $dirs \
  | egrep '\.scala$' \
  | xargs scalac \
    -Dscala.timings=true \
    -unchecked -deprecation -cp $libs  \
    -d $scriptDir/bin/ \
  | $scriptDir/color_scalac.awk

echo >&2 "Building JAR..."
(cd $scriptDir/bin; zip -qr $scriptDir/ducttape.jar *)

#echo >&2 "Building One JAR to Rule Them All..."
#java -jar $scriptDir/lib/proguard-4.7.jar @ducttape.proguard
