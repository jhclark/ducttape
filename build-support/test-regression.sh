#!/usr/bin/env bash
set -eo pipefail
scriptDir=$(cd $(dirname $0); pwd) # works on mac osx too
rootDir=$scriptDir/..

# NOTE: This script requires ducttape to be in PATH
# (see Makefile)

echo >&2 $PATH
DUCTTAPE=$(which ducttape)

tutorialDir=$(cd $rootDir/tutorial; pwd)
for tape in $tutorialDir/*.tape; do
    dir=$(dirname $tape)
    basefile=$(basename $tape .tape)
    customSh=$dir/$basefile.sh
    if [ -e $customSh ]; then
        # Run via the custom script, which may give additional arguments such as a .conf file
        CMD=$customSh
    else
        # Just directly execute the .tape file with ducttape
        CMD="$DUCTTAPE $tape"
    fi
    echo "==================="
    echo "Running test: $CMD"
    echo "==================="
    cd $dir
    $CMD -y purge '*' '*'
    $CMD -y
done
