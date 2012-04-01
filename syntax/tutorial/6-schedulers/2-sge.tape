# * The resource parameter vmem can be specified as .vmem at task declarations
# * The "cmds" parameter is inserted as a direct string replacement by ducttape
#   and contains the "payload" of this task
# * This assumes that scheduler submission happens asynchronously
#   and that we must poll the scheduler to learn when the job has completed
submitter sge :: cmds vmem walltime {
  action wrap > wrapper {
    #$-l vmem=$vmem
    #$-l walltime=$walltime
    $cmds
  }
  action run < wrapper > id {
    qsub $wrapper > $id
  }
  action poll > done exit_code {
    TODO # insert the lengthy script needed to properly get the exit code here
  }
  action status > short_status long_status {
    qstat -f $id | awk '/job_state = Q/{print "queued"} /job_state = R/{print "running"}' > $status
  }
}

# NOTE: File transfers and decompression are handled by
# a per-machine limit for each of these tasks and are
# but are not included in the submit-script time
