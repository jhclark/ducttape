#!/usr/bin/env bash
set -eo pipefail
scriptDir=$(dirname $0)

java -Dfile.encoding=UTF8 \
     -Xmx1536M \
     -Xss1M \
     -XX:+CMSClassUnloadingEnabled \
     -XX:MaxPermSize=384m \
     -jar $scriptDir/sbt-launch.jar \
     "$@" \
     1>&2 # Hack for Travis CI
