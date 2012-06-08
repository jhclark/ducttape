#!/usr/bin/env bash
set -ueo pipefail
#scriptDir=$(scala -e 'println(new java.io.File("'$(dirname $0)'").getAbsolutePath)')
scriptDir=$(readlink -f $(dirname $0))
rootDir=$scriptDir/..

echo >&2 "Building JAR..."
(cd $rootDir/target/scala-2.9.2/classes; zip -qr $rootDir/ducttape.jar *)
