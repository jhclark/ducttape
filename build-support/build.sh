#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(cd $(dirname $0); pwd)
rootDir=$scriptDir/..
libDir=$rootDir/lib

fast=""
if (( $# == 1 )); then
    fast=$1
fi

ZINC_OPTS="-scala-home $SCALA_HOME"

# This line will also make sure SCALA_HOME is defined thanks to set -u
echo >&2 "Using Scala install at $SCALA_HOME (Note: 2.10+ is required)"

# Include both webui and test JARs
libs=$(find $rootDir/lib/ -iname '*.jar')
# Delimit with colon
classpath=$(echo "$libs" | paste -s -d':')

echo >&2 "Using Zinc at $(which zinc) (Note: 0.1.4+ is required)"

echo >&2 "Building source using zinc..."
mkdir -p $rootDir/bin
find $rootDir/src/main/scala $rootDir/src/test/scala -iname '*.scala' \
  | egrep '\.scala$' \
  | xargs zinc $ZINC_OPTS \
    -cp $classpath \
    -d $rootDir/bin/zinc \
    -S-unchecked -S-deprecation -nailed -analysis-cache $rootDir/bin/zinc.cache

echo >&2 "Building initial JAR..."
(cd $rootDir/bin/zinc; zip -qr $rootDir/ducttape.tmp.jar *)

jarPath=$rootDir/ducttape.jar
if [[ "$fast" == "--fast" ]]; then
    echo >&2 "WARNING: Using fast (potentially incorrect) JAR builder; if you encounter errors, build again without --fast"
    cd $rootDir/bin/zinc
    zip -qu $jarPath *
else
    echo >&2 "Minimizing JAR using proguard..."
    # Reads $rootDir/ducttape.tmp.jar as an intermediate input and writes $rootDir/ducttape.jar
    time java -jar $scriptDir/proguard-4.8.jar @$scriptDir/proguard.ini
    
    # Add version.info
    (cd $rootDir; zip -gqr $jarPath version.info)
    rm $rootDir/ducttape.tmp.jar
fi
echo >&2 "Wrote JAR: $jarPath"
