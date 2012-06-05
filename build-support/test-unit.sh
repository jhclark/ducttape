#!/usr/bin/env bash
set -eo pipefail
scriptDir=$(cd $(dirname $0); pwd)
rootDir=$scriptDir/..

cd $rootDir
JAVA_OPTS="-XX:MaxJavaStackTraceDepth=7" \
    scala \
    -cp lib/test/scalatest-1.7.1.jar:lib/grizzled-slf4j_2.9.1-1-0.6.8.jar:ducttape.jar \
    org.scalatest.tools.Runner \
    -p . -o \
    -s ducttape.hyperdag.walker.UnpackedDagWalkerTest \
    -s ducttape.hyperdag.walker.UnpackedDagWalkerTest
