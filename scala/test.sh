#!/usr/bin/env bash
set -eo pipefail

scalac -cp lib/scalatest-1.4.1/scalatest-1.4.1.jar test.scala hyperdag.scala agenda.scala types.scala

scala -cp lib/scalatest-1.4.1/scalatest-1.4.1.jar org.scalatest.tools.Runner -p . -o -s ExampleSpec
