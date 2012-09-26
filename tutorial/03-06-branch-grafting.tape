# Chapter 3: HyperWorkflows
# Lesson 6:  Branch Grating

# Branch points allow you to run a single task and all of its dependents
# under a particular set of conditions (the realization). But what
# if you want some dependents to only use a specific configuration?
# For this, ducttape provides branch grafts.
#
# A branch graft constrains an input or parameter to use only realizations
# that contain the specified branch(es) -- this of course requires that the branch
# has been defined by the parent task or one of its dependencies. The task at which
# the graft was requested will then have no further visibility of that
# branch point -- nor will any dependents of that task have visibility
# of the branch point. 
#
# Note: You can graft multiple branches (from different branch points) using comma
# we don't show an example of this for brevity.
# Note: One input may use a graft and while another input does not use a graft. We
# don't show this usage here.
#
# A use case from natural language processing for branch grafts:
# We have a tokenizer that we'd like to run on the training, dev, and test set
# and then later using exactly one branch from that branch point

task preproc < in=(DataSet: train=big.txt test=small.txt) > out {
  cat < $in > $out
}

task trainer < in=$out@preproc[DataSet:train] > model {
   cat < $in > $model
}

task tester < in=$out@preproc[DataSet:test] > hyps {
  cat < $in > $hyps
}
