# This file is for systems such as PBS/Torque with the Built-in, Catalina, or Maui schedulers.

# * The resource parameter vmem can be specified as .vmem at task declarations
# * The "cmds" parameter is inserted as a direct string replacement by ducttape
#   and contains the "payload" of this task
# * This assumes that scheduler submission happens asynchronously
#   and that we must poll the scheduler to learn when the job has completed
submitter sge :: cpus vmem walltime q // these can be passed as parameters to each task: .cpus .vmem .walltime .q
              :: COMMANDS // the bash commands from some task 
              :: TASK REALIZATON CONFIGURATION // variables passed by ducttape 
              {
  # TODO: We're still missing a few variables that typically get passed in.
  # SGE: echo "#$-l vmem=$vmem" >> $wrapper
  # SGE:  echo "#-l walltime=$walltime" >> $wrapper
  action run > jobid {
    wrapper="job.sh"
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

  # Can we divide this into 3 parts?
  # 1) Global queue polling [get_queue]
  # 2) Parse an in-memory (or tmp) version of the queue for each task [check_job]
  # XXX) Back-off to expensive tracejob calls only when that fails? 
  # Can we remove need to tracejob iff internal worker always writes exit code on success
  # But then how do we recover non-zero exit codes?

  # TODO: Specify that input and output files may be read and written *once*
  # i.e. they are *stream* descriptors

  # If we can't get the queue... just keep retrying and print a warning.
  # We can't do anything until we know if jobs are running though.
  action get_queue > q {
    qstat > $q
  }

  # Can ducttape check exit code before doing this? Or is that bad for the FS?
  action check_job < jobid q > done {
    # If job not queued or running, it's done
    line=$(fgrep $jobid $q) 
    # The Shepard that ran the job is responsible for writing an exit_code file
    # That will determine success (Uses shutdown hook to catch signals)
    # However, we still can't catch signal 9, which is what most schedulers use to kill
    # Do we give an error message such as "your job may have been killed"?
  }

  # Ducttape will run this once every n seconds
  # TODO: Is there any way to get around this silly jobid file?
  # TODO: Can we have one function for grabbing the qsub output globally and another for just this job?
  action poll < jobid > done exit_code {
   # Note: This is complicated by having to poll rather than being able to just use SGE's -sync
   # It also allows ducttape to "resume" jobs submitted to the scheduler if a gateway node goes down

   jobid=$(cat $jobid | cut -d. -f1) # Remove server name, if specified
   [[ "$jobid" != "" ]] || (echo >&2 "ERROR: Empty job id. Did the job fail to submit?"; exit 1)

   # Use -alm to avoid costly queries to logs we don't even need
   exit_status=$(tracejob -alm $jobid | awk '/Exit_status=-?[0-9]+/{print $4}' | cut -d= -f2)
   if [[ "$exit_status" != "" ]]; then
     echo >&2 "Job exited with status $exit_status" # Ducttape has stdout/stderr
     exit $exit_status
   fi
  }

  action status > short_status long_status {
    qstat -f $id | awk '/job_state = Q/{print "queued"} /job_state = R/{print "running"}' > $status
  }
}

# NOTE: File transfers and decompression are handled by
# a per-machine limit for each of these tasks and are
# but are not included in the submit-script time
