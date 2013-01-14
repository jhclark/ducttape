#!/usr/bin/env bash
set -ueo pipefail
set -x
scriptDir=$(cd $(dirname $0); pwd)

if [[ "$CI" == true ]]; then
  # Travis only has SBT, but we want the latest Zinc and Scala distribution, so just download them
  wget http://repo.typesafe.com/typesafe/zinc/com/typesafe/zinc/dist/0.1.4/zinc-0.1.4.tgz
  tar -xvzf zinc-0.1.4.tgz
  export PATH=$PWD/zinc-0.1.4/bin:$PATH
  wget http://www.scala-lang.org/downloads/distrib/files/scala-2.10.0.tgz
  tar -xvzf scala-2.10.0.tgz
  export SCALA_HOME=$PWD/scala-2.10.0
  export PATH=$SCALA_HOME/bin:$PATH
fi

$scriptDir/build.sh
$scriptDir/test-unit.sh
$scriptDir/ensure_unique_tutorial_tasks.sh
if [[ "$TRAVIS" == true ]]; then
    # If we're running on Travis, go ahead and install the necessary latex packages to test
    # documentation generation
    sudo apt-get install -qq texlive-latex-base texlive-fonts-recommended texlive-science
fi
$scriptDir/doc.sh
$scriptDir/dist.sh
$scriptDir/test-regression.sh $PWD/dist/ducttape-current
