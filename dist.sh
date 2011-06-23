#!/usr/bin/env bash
set -eo pipefail
scriptDir=$(dirname $0)

cd $scriptDir
mkdir -p dist/ducttape
if [ ! -e lib/scala-library.jar ]; then
  echo >&2 "Please place copy $SCALA_INSTALL/lib/scala-library.jar to lib/"
  exit 1
fi
cp -r README ducttape ducttape.jar lib dist/ducttape
cd dist
tar -cvzf ducttape.tgz ducttape
