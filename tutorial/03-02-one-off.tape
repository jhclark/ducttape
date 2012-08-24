# Chapter 3: HyperWorkflows
# Lesson 2:  The default one off plan
#
# Most HyperWorkflows involve more than one branch point. With a large
# enough number of these, running the cross-product of all branches
# quickly becomes infeasible.
#
# To address this problem, ducttape provides "plans". A plan is a set
# of branches that the user wishes to be run at a specific task.
# By default, ducttape will execute all tasks, but only for realizations
# that contain no more than one *non-baseline* branch.
#
# In this example, we will see how to run these variations as
# "one off" experiments.

# We start with a task much like the previous example
task one_off_1 < in=(WhichSize: smaller=small.txt bigger=big.txt) > out {
  cat < $in > $out
}

# And now add a child task that also has a packed parameter with multiple branches
# a subdirectory will be created under the task directory for one_off_2
# for each realization: one-smaller, one-bigger, two-smaller, two-bigger
#
# Notice that the realization directory name is formed by first sorting
# the active branches by their branch point names and then removing
# any "baseline" branches
task one_off_2 < in=$out@one_off_1 :: N=(N: one=1 two=2) {
  head -n $N < $in
}

# By default, ducttape will run each of these as "one off" experiments: Ducttape
# runs each workflow to completion (still sharing as much substructure as possible)
# using each branch of every Branch Point, but using the baseline branch for all other Packs.
# With the default One Off strategy, the following 4 directories will result from running
# this HyperWorkflow:
# * ./one_off_1/Baseline.baseline/1
# * ./one_off_1/WhichSize.bigger/1 
# * ./one_off_2/Baseline.baseline/1 (with the baseline branch "small" as one-off-1/in)
# * ./one_off_2/N.two/1 (with the baseline branch "small" as one-off-1/in)
