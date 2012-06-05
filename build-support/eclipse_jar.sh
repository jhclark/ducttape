#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(cd $(dirname $0); pwd)
rootDir=$scriptDir/..

echo >&2 "Building JAR..."
(cd $rootDir/bin; zip -qr $rootDir/ducttape.jar *)
