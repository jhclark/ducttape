#!/usr/bin/env bash
set -eo pipefail
scriptDir=$(dirname $0)

echo "Running tests from: $dir"
for dir in dirs; do
    for tape in syntax/tutorial/*/*.tape; do
        echo "Running test: $dir"
        $scriptDir/ducttape $tape -y purge '*' '*'
        $scriptDir/ducttape $tape -y
    done
done
