#!/usr/bin/env bash
set -eo pipefail
scriptDir=$(dirname $0)

dir=syntax/tutorial/1-basics

echo "Running tests from: $dir"
for tape in $dir/*.tape; do
    echo "Running test: $dir"
    $scriptDir/ducttape $tape
done
