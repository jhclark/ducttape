#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(cd $(dirname $0); pwd)
rootDir=$scriptDir/..

cd $rootDir
libs=$(ls lib/*.jar lib/webui/*.jar | awk '{printf $0":"}')
mkdir -p scaladoc
find src/main/scala | egrep '[^.].*\.scala$' | xargs scaladoc -d scaladoc -cp $libs
