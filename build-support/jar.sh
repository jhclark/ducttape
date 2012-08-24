#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(cd $(dirname $0); pwd)
rootDir=$scriptDir/..

echo >&2 "Building JAR..."
(cd $rootDir/target/scala-2.9.2/classes; zip -qr $rootDir/ducttape.jar *)
(cd $rootDir; zip -gqr $rootDir/ducttape.jar version.info)
