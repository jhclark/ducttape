# Chapter 3: HyperWorkflows
# Lesson 1:  What are HyperWorkflows?
#
# Experimentation often requires one to run similar sequences of tasks
# in a variety of configurations. To compactly represent such workflows,
# ducttape provides HyperWorkflows.
#
# For example, if we want to determine the effect of data size on some task,
# we could run the task several times, using input files of differing sizes,
# as below.


# This step shows the simplest way to pack (i.e. create a hyperworkflow)
# Parentheses with "fileName=(packName: a=x, b=y)" indicates that we will
# have branches "a" and "b" for file "f" and the name of the Branch Point
# (aka Pack) will be "packName".
# The file "x" will be used for branch a and the file "y"
# will be used when the branch b is active.
#
# * This task will result in two output directories:
#   ./has-branches/baseline/1/work
#   ./has-branches/bigger/1/work
#
# Because a is the first branch, it is considered the Baseline branch.
# Therefore, it is given the special directory name "baseline". This
# will become important later.
#
# Because there is a task with more than one Branch, we say that
# this workflow has more than one Realization (which can be thought of
# as a single DAG derivation of this HyperDAG).
#
# Relative paths *as inputs* are resolved relative to this .tape file.
# (Remember, relative paths as outputs are resolved relative to the task's working directory)
task has_branches < in=(WhichSize: smaller=small.txt bigger=big.txt) > out {
  cat < $in > $out
}


# Since its parent task has several branches (smaller and bigger),
# this task will be run once for each parent branch.
#
# * This task will also result in two output directories:
#   ./parent-has-branches/baseline/1/work
#   ./parent-has-branches/bigger/1/work
task parent_has_branches < in=$out@has_branches {
  wc -l $in
}
