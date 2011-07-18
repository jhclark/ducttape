#!/usr/bin/env bash
set -eo pipefail

dir=syntax/tutorial/1-basics

echo "Running tests from: $dir"
for tape in $dir/*.tape; do
    echo "Running test: $dir"
    ducttape $tape
done
