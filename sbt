#!/usr/bin/env bash
set -eo pipefail
scriptDir=$(dirname $0)

if [ ! -e $scriptDir/lib/sbt-launch.jar ]; then
  wget http://typesafe.artifactoryonline.com/typesafe/ivy-releases/org.scala-tools.sbt/sbt-launch/0.10.1/sbt-launch.jar -o $scriptDir/lib/sbt-launch.jar
fi

java -Dfile.encoding=UTF8 \
     -Xmx512M \
     -Xss1M \
     -XX:+CMSClassUnloadingEnabled \
     -XX:MaxPermSize=256m \
     -jar $scriptDir/lib/sbt-launch.jar \
     "$@"
