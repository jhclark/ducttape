#!/usr/bin/env bash
set -euo pipefail

scala -cp lib/servlet-api-3.0.jar:lib/sqlitejdbc-v056.jar:lib/jetty-all-8.0.4.v20111024.jar:ducttape.jar \
    WebServer