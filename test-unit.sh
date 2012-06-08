#!/usr/bin/env bash
set -eo pipefail
scriptDir=$(dirname $0)

libs=$scriptDir/bin
#libs=$scriptDir/ducttape.jar
libs=$libs:$scriptDir/lib/scala-library-2.9.2.jar
libs=$libs:$scriptDir/lib/commons-lang3-3.1.jar
libs=$libs:$scriptDir/lib/commons-io-2.2.jar
libs=$libs:$scriptDir/lib/grizzled-slf4j_2.9.1-1-0.6.8.jar
libs=$libs:$scriptDir/lib/slf4j-api-1.6.4.jar
libs=$libs:$scriptDir/lib/slf4j-jdk14-1.6.4.jar

libs=$libs:$scriptDir/lib/test/scalatest-1.7.1.jar

#JAVA_OPTS="-XX:MaxJavaStackTraceDepth=7" \
java -cp $libs \
    org.scalatest.tools.Runner \
    -p . -o \
    -s ducttape.hyperdag.walker.UnpackedDagWalkerTest \
    -s ducttape.syntax.TaskSpecTest \
    -s ducttape.syntax.BranchPointTest
