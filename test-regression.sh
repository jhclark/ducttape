#!/usr/bin/env bash
set -eo pipefail
scriptDir=$(readlink -f $(dirname $0))

# We don't want to assume ducttape is in the path for running test scripts
export PATH=$scriptDir:$PATH

tutorialDir=$(readlink -f syntax/tutorial)
for tape in $tutorialDir/*/*.tape; do
    dir=$(dirname $tape)
    basefile=$(basename $tape .tape)
    customSh=$dir/$basefile.sh
    if [ -e $customSh ]; then
        # Run via the custom script, which may give additional arguments such as a .conf file
        CMD=$customSh
    else
        # Just directly execute the .tape file with ducttape
        CMD="$scriptDir/ducttape $tape"
    fi
    echo "==================="
    echo "Running test: $CMD"
    echo "==================="
    cd $dir
    $CMD -y purge '*' '*'
    $CMD -y
done
