#!/usr/bin/env bash
set -eo pipefail
scriptDir=$(cd $(dirname $0); pwd)
rootDir=$scriptDir/..

echo >&2 "Testing classes in $rootDir/bin/zinc"
libs=$rootDir/bin/zinc:$(find $rootDir/lib -iname '*.jar' | paste -s -d':')

#JAVA_OPTS="-XX:MaxJavaStackTraceDepth=7" \
# Note -p from 1.7 is now -R in 2.0
java -cp $libs \
    org.scalatest.tools.Runner \
    -R . -o \
    -s ducttape.hyperdag.walker.UnpackedDagWalkerTest \
    -s ducttape.syntax.TaskSpecTest \
    -s ducttape.syntax.BranchPointTest
