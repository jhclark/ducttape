#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(cd $(dirname $0); pwd)

# Reads $rootDir/ducttape.tmp.jar as an intermediate input
time java -jar $scriptDir/proguard-4.8.jar @$scriptDir/proguard.ini

rootDir=$scriptDir/..
jarPath=$rootDir/ducttape.jar
(cd $rootDir; zip -gqr $jarPath version.info)
