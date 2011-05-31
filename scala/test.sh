#!/usr/bin/env bash
set -eo pipefail

mkdir -p bin/
fsc -cp lib/scalatest-1.4.1/scalatest-1.4.1.jar \
    -d bin/ *.scala

JAVA_OPTS="-XX:MaxJavaStackTraceDepth=7" scala -cp lib/scalatest-1.4.1/scalatest-1.4.1.jar:bin/ \
    org.scalatest.tools.Runner \
    -p . -o \
    -s PackedDagWalkerTest \
    -s UnpackedDagWalkerTest
