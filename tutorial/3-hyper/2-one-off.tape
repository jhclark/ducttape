# Most HyperWorkflows involve more than one dimension of experimentation.
# That is, there is more than one packed file/parameter among the workflow's
# tasks. In this example, we will see how to run these variations as
# "one off" experiments.

# We start with a task much like the previous example
task one_off_1 < in=(WhichSize: smaller=small.txt bigger=big.txt) > out {
  cat < $in > $out
}

# And now add a child task that also has a packed parameter with multiple branches
# a subdirectory will be created under the task directory for one_off_2
# for each realization: one-smaller, one-bigger, two-smaller, two-bigger
task one_off_2 < in=$out@one_off_1 :: N=(N: one=1 two=2) {
  head -n $N < $in
}

# By default, ducttape will run each of these as "one off" experiments: Ducttape
# runs each workflow to completion (still sharing as much substructure as possible)
# using each branch of every Pack, but using the baseline branch for all other Packs.
# With the default One Off strategy, the following 4 directories will result from running
# this HyperWorkflow:
# * ./one-off-1/baseline/1
# * ./one-off-1/bigger/1 
# * ./one-off-2/baseline/1 (with the baseline branch "small" as one-off-1/in)
# * ./one-off-2/two/1 (with the baseline branch "small" as one-off-1/in)
