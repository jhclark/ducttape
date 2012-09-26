#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(cd $(dirname $0); pwd)

$scriptDir/sbt proguard

rootDir=$scriptDir/..
jarPath=$rootDir/ducttape.min.jar
(cd $rootDir; zip -gqr $jarPath version.info)
