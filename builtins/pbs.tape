# NOTE: You may need to copy this into your own workflow
# and modify it on a per-cluster basis as the specific
# resource options and queue names tend to vary from
# cluster to cluster
submitter pbs :: walltime cpus vmem
              :: COMMANDS
              :: TASK REALIZATION CONFIGURATION {
  action run {
    wrapper="ducttape_job.sh"

    echo "#PBS -S /bin/bash" >> $wrapper
    echo "#PBS -q all.q" >> $wrapper
    echo "#PBS -l nodes=1:ppn=$cpus" >> $wrapper
    echo "#PBS -l vmem=$vmem" >> $wrapper
    echo "#PBS -l walltime=$walltime" >> $wrapper
    echo "#PBS -j oe" >> $wrapper
    echo "#PBS -o localhost:$PWD/ducttape_job_out.txt" >> $wrapper
    # There's a limit on how long task names can be in PBS
    echo "#PBS -N $(echo $CONFIGURATION/$TASK/$REALIZATION | head -c200)" >> $wrapper

    echo "#PBS -m bea" >> $wrapper
    echo "#PBS -V" >> $wrapper

    # Bash flags aren't necessarily passed into the scheduler
    # so we must initialize them
    echo "set -e # stop on errors" >> $wrapper
    echo "set -o pipefail # stop on pipeline errors" >> $wrapper
    echo "set -u # stop on undeclared variables" >> $wrapper
    echo "set -x # show each command as it is executed" >> $wrapper

    # The current working directory will also be changed
    echo "cd $PWD" >> $wrapper
    echo >> $wrapper

    # NOTE: Commands will be literally substituted here by ducttape, not bash
    echo "$COMMANDS" >> $wrapper

    # Can be uncommented on systems with lightweight PBS servers
    # Eventually, this should be handled in ducttape itself
    #sec=$(($RANDOM % 30))
    #echo >&2 "Submitting in $sec sec..."
    #sleep $sec # Don't spam qsub

    qsub $wrapper > ducttape_torque_jobid.txt
    sleep 10 # Give the scheduler some time to begin showing the job in qstat

    # TODO: XXX: FIXME: implement new submitter framework that handles Torque better
    UGLY_HARDCODED_DUCTTAPE_DIR=/home/jhclark/software/ducttape/builtins
    # TODO: Move this into new and improved submitter framework, once it's implemented
    $UGLY_HARDCODED_DUCTTAPE_DIR/pbs-wait-for-job.sh \
        $(cat ducttape_torque_jobid.txt) \
        ducttape_job_out.txt \
        ducttape_job.info
  }
}
