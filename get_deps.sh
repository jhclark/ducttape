#!/usr/bin/env bash
set -eo pipefail
scriptDir=$(dirname 0)

cd $scriptDir/lib/
scalatest=scalatest-1.6.1
if [ ! -e $scalatest.jar ]; then
    echo >&2 "Retrieving library $scalatest"
    scalatestUrl=http://www.scalatest.org/releases/$scalatest.zip
    tmp=$(mktemp)
    curl -L $scalatestUrl > $tmp
    unzip $tmp
    cp $scalatest/$scalatest.jar .
    rm -rf $scalatest $tmp
fi
cd $scriptDir
