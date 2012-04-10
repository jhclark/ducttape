# This file is for systems such as PBS/Torque with the Built-in, Catalina, or Maui schedulers.

# * The resource parameter vmem can be specified as .vmem at task declarations
# * The "cmds" parameter is inserted as a direct string replacement by ducttape
#   and contains the "payload" of this task
# * This assumes that scheduler submission happens asynchronously
#   and that we must poll the scheduler to learn when the job has completed
# * These files should be treated as streams and never used for repeated nor random access
submitter sge :: cpus vmem walltime q /* these can be passed as parameters to each task: .cpus .vmem .walltime .q */
              :: COMMANDS /* the bash commands from some task */
              :: TASK REALIZATON CONFIGURATION /* variables passed by ducttape */ {
  # TODO: We're still missing a few variables that typically get passed in.
  # SGE: echo "#$-l vmem=$vmem" >> $wrapper
  # SGE:  echo "#-l walltime=$walltime" >> $wrapper
  action submit > jobid {
    wrapper="ducttape_torque_job.sh"
    echo "#PBS -S /bin/bash" >> $wrapper
    echo "#PBS -q $q" >> $wrapper
    echo "#PBS -l nodes=1:ppn=$cpus" >> $wrapper
    echo "#PBS -l walltime=$walltime" >> $wrapper
    echo "#PBS -j oe" >> $wrapper
    echo "#PBS -o localhost:$PWD/job.out" >> $wrapper
    echo "#PBS -N $CONFIGURATION/$TASK/$REALIZATION" >> $wrapper

    # Bash flags aren't necessarily passed into the scheduler
    # so we must re-initialize them
    echo "set -e # stop on errors" >> $wrapper
    echo "set -o pipefail # stop on pipeline errors" >> $wrapper
    echo "set -u # stop on undeclared variables" >> $wrapper
    echo "set -x # show each command as it is executed" >> $wrapper

    # The current working directory will also be changed by most schedulers
    echo "cd $PWD/work" >> $wrapper

    echo "$COMMANDS" >> $wrapper

    qsub $wrapper > $jobid
  }

  # If we can't get the queue... just keep retrying and print a warning.
  # We can't do anything until we know if jobs are running though.
  action get_queue > q {
    qstat > $q
  }

  # Can ducttape check exit code before doing this? Or is that bad for the FS?
  action check_job < jobid q > short_status long_status done {
    # If job not queued or running, it's done
    line=$(fgrep $jobid $q) 
   # This allows ducttape to "resume" jobs submitted to the scheduler if a gateway node goes down

    qstat -f $id | awk '/job_state = Q/{print "queued"} /job_state = R/{print "running"}' > $status
  }

  # Ducttape will run this only after check_job says that the job has finshed
  # Now we see if it failed or succeeded
  action poll < jobid > exit_code {
   jobid=$(cat $jobid | cut -d. -f1) # Remove server name, if specified
   [[ "$jobid" != "" ]] || (echo >&2 "ERROR: Empty job id. Did the job fail to submit?"; exit 1)

   # Use -alm to avoid costly queries to logs we don't even need
   tracejob -alm $jobid | awk '/Exit_status=-?[0-9]+/{print $4}' | cut -d= -f2 > $exit_code
  }
}

# NOTE: File transfers and decompression are handled by
# a per-machine limit for each of these tasks and are
# but are not included in the submit-script time
