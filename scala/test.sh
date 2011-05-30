#!/usr/bin/env bash
set -eo pipefail

mkdir -p bin/
fsc -cp lib/scalatest-1.4.1/scalatest-1.4.1.jar \
    -d bin/ \
    test.scala viz.scala util.scala agenda.scala types.scala multiset.scala hyperdag.scala

scala -cp lib/scalatest-1.4.1/scalatest-1.4.1.jar:bin/ \
    org.scalatest.tools.Runner \
    -p . -o \
    -s PackedDagWalkerTest
