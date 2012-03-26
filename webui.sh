#!/usr/bin/env bash
set -euo pipefail
scriptDir=$(dirname $0)

JAVA_OPTS='-Xmx128m' scala -cp $scriptDir/lib/servlet-api-3.0.jar:$scriptDir/lib/sqlitejdbc-v056.jar:$scriptDir/lib/jetty-all-8.0.4.v20111024.jar:$scriptDir/ducttape.jar \
    WebServer $@