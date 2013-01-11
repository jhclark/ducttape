#!/usr/bin/env bash
set -eo pipefail
scriptDir=$(cd $(dirname $0); pwd) # works on mac osx too
rootDir=$scriptDir/..

# Allow user to specify which ducttape directory to test so that we can test the packaged distribution on Travis
if (( $# > 1 )); then
    DUCTTAPE_DIR=$1
else
    DUCTTAPE_DIR=$rootDir
fi

# Make sure we're testing the version of ducttape in our current environment
function test_all {
  echo >&2 $PATH
  DUCTTAPE=$DUCTTAPE_DIR/ducttape
  export PATH=$DUCTTAPE_DIR:$PATH

  export PATH=$rootDir:$PATH

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
    output=$(mktemp -d $tape.regression.TMP.XXXXXX)
    echo "==================="
    echo "Running test: $CMD"
    echo "Output: $output"
    echo "==================="
    cd $dir
    $CMD -y -O $output

    # Remove output, if test was successful
    rm -rf $output
  done
}

time test_all
GREEN="\033[0;32m"
RESET="\033[0m"
echo -e "${GREEN}ALL REGRESSION TESTS PASSED${RESET}"
