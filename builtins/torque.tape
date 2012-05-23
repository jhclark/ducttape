# cpus, vmem, walltime, q: these can be passed as parameters to each task: .cpus .vmem .walltime .q
# COMMANDS: the bash commands from some task
# TASK, REALIZATION, CONFIGURATION: variables passed by ducttape
submitter torque :: vmem walltime q
                 :: COMMANDS
                 :: TASK REALIZATON CONFIGURATION {
  action run {
    wrapper="ducttape_job.sh"

    echo "#PBS -S /bin/bash" >> $wrapper
    echo "#PBS -q $q" >> $wrapper
    echo "#PBS -l nodes=1:ppn=$cpus" >> $wrapper
    echo "#PBS -l walltime=$walltime" >> $wrapper
    echo "#PBS -j oe" >> $wrapper
    echo "#PBS -o localhost:$PWD/job.out" >> $wrapper
    echo "#PBS -N $CONFIGURATION/$TASK/$REALIZATION" >> $wrapper

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

    echo "$COMMANDS" >> $wrapper

    echo "qsub $wrapper > jobid.txt"

    # TODO: XXX: FIXME: implement new submitter framework that handles Torque better
    UGLY_HARDCODED_DUCTTAPE_DIR=/home/jhclark/software/ducttape/builtins
    # TODO: Move this into new and improved submitter framework, once it's implemented
    echo "$UGLY_HARDCODED_DUCTTAPE_DIR/pbs-wait-for-job.sh \$(cat jobid.txt)"
  }
}
