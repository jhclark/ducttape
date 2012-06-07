#!/usr/bin/env bash
set -ueo pipefail
scriptDir=$(cd $(dirname $0); pwd)
tutorialDir=$scriptDir/../tutorial

cd $tutorialDir/
egrep -Hno '^task [^ ]+' *.tape \
    | awk -F' ' 'BEGIN{err=0} {if(seen[$2]) { print "Nonunique task name: "$0" "; print "Previously saw: "seen[$2]; print ""; err+=1}; seen[$2]=$0} END{exit err}'
