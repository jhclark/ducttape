#!/usr/bin/env bash
set -eo pipefail

JAVA_OPTS="-XX:MaxJavaStackTraceDepth=7" scala -cp lib/scalatest-1.6.1.jar:bin/ \
    org.scalatest.tools.Runner \
    -p . -o \
    -s ducttape.syntax.CommentTest \
    -s ducttape.syntax.VariableNameTest \
    -s ducttape.syntax.BranchPointNameTest \
#    -s ducttape.syntax.TaskNameTest
