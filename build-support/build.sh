#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(scala -e 'println(new java.io.File("'$(dirname $0)'").getAbsolutePath)')
rootDir=$scriptDir/..
libDir=$rootDir/lib

# NOTE: This build file is intended primarily for builds
# where SBT cannot be used (i.e. there is no Internet connection)
# Subtitle: If you're not Lane, consider just running "sbt"

libs=""
libs="$libs:$libDir/sqlitejdbc-v056.jar"
libs="$libs:$libDir/scala-optparse-1.1.jar"
libs="$libs:$libDir/commons-lang3-3.1.jar"
libs="$libs:$libDir/commons-io-2.2.jar"
libs="$libs:$libDir/grizzled-slf4j_2.9.1-1-0.6.8.jar"
libs="$libs:$libDir/pegdown-1.1.0.jar"
libs="$libs:$libDir/parboiled-core-1.0.2.jar"
libs="$libs:$libDir/parboiled-java-1.0.2.jar"
libs="$libs:$libDir/webui/servlet-api-3.0.jar"
libs="$libs:$libDir/webui/jetty-all-8.0.4.v20111024.jar"

libs="$libs:$libDir/test/junit-4.10.jar"
libs="$libs:$libDir/test/scalatest-1.7.1.jar"

echo >&2 "Building source..."
mkdir -p $rootDir/bin
find $rootDir/src/main/scala $rootDir/src/test/scala \
  | egrep '\.scala$' \
  | xargs scalac \
    -Dscala.timings=true \
    -unchecked -deprecation -cp $libs  \
    -d $rootDir/bin/ \
  | $rootDir/build-support/color_scalac.awk

echo >&2 "Building JAR..."
(cd $rootDir/bin; zip -qr $rootDir/ducttape.jar *)

