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

echo >&2 "Job queued at $(date)"

prev_job_state=""
while true; do
    # Use -alm to avoid costly queries to logs we don't even need
    #exit_status=$(tracejob -alm $jobid | awk '/Exit_status=-?[0-9]+/{print $4}' | cut -d= -f2)
    # tracejob disabled on trestles

    unk=$(qstat -f $jobid | awk -F' += +' '/Unknown/{print "1"}')
    if [[ "$unk" == "1" ]]; then
	echo >&2 "Job can no longer be found in qstat"
	exit 1
    fi

    job_state=$(qstat -f $jobid | awk -F' += +' '/job_state/{print $2}')
    exit_status=$(qstat -f $jobid | awk -F' += +' '/exit_status/{print $2}')

    if [[ $job_state == "R" && $prev_job_state != "R" ]]; then
        echo >&2 "Job began running at $(date)"
    fi

    if [[ $exit_status != "" ]]; then
        echo >&2 "Job finished at $(date)"
	echo >&2 "Job exited with status $exit_status"
	if [[ -e $errFile ]]; then
	    cat $errFile
	fi
        exit $exit_status
    fi
    prev_job_state=$job_state
    sleep 30
done