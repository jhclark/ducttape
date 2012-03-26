#!/usr/bin/env bash                                                                                          
set -e
set -o pipefail

jobid=$1
errFile=$2

# Remove server name, if specified
jobid=$(echo $jobid | cut -d. -f1)

if [[ $jobid == "" ]]; then
    echo >&2 "ERROR: Empty job id. Did the job fail to submit?"
    exit 1
fi

while true; do
    # Use -alm to avoid costly queries to logs we don't even need
    exit_status=$(tracejob -alm $jobid | awk '/Exit_status=-?[0-9]+/{print $4}' | cut -d= -f2)
    if [[ $exit_status != "" ]]; then
	echo >&2 "Job exited with status $exit_status"
	if [[ -e $errFile ]]; then
	    cat $errFile
	fi
        exit $exit_status
    fi
    sleep 30
done