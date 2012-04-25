#!/usr/bin/env bash
set -eo pipefail

JAVA_OPTS="-XX:MaxJavaStackTraceDepth=7" \
    scala \
    -cp lib/test/scalatest-1.7.1.jar:ducttape.jar \
    org.scalatest.tools.Runner \
    -p . -o \
    -s PackedDagWalkerTest \
    -s UnpackedDagWalkerTest
