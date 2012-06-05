#!/usr/bin/env bash
set -ueo pipefail

libs=$(ls lib/*.jar lib/webui/*.jar | awk '{printf $0":"}')
mkdir -p scaladoc
find src/main/scala | egrep '[^.].*\.scala$' | xargs scaladoc -d scaladoc -cp $libs
