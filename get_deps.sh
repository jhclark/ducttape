#!/usr/bin/env bash
set -eo pipefail
scriptDir=$(dirname 0)

#mkdir -p $scriptDir/lib/
#cd $scriptDir/lib/
#scalatest=scalatest-1.6.1
#if [ ! -e $scalatest.jar ]; then
#    echo >&2 "Retrieving library $scalatest"
#    scalatestUrl=http://www.scalatest.org/releases/$scalatest.zip
#    tmp=tmp.zip
#    curl -L $scalatestUrl > $tmp
#    unzip $tmp
#    cp $scalatest/$scalatest.jar .
#    rm -rf $scalatest $tmp
#fi

if [ ! -e lib/scala-library.jar ]; then
  scalaDir=$(dirname $(which scala))/../
  echo >&2 "Scala found at $scalaDir"
  jar=$scalaDir/lib/scala-library.jar
  echo >&2 "Copying scala library from $jar for use in packaging"
  cp $jar lib/
fi
