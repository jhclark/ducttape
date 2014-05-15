# Chapter 3: HyperWorkflows
# Lesson 3:  Custom realization plans

# We start with the same example from part 2
task planned_1 < in=(WhichSize: smaller=small.txt bigger=big.txt) > out {
  cat < $in > $out
}

# And add a sequence to this step
task planned_2 < in=$out@planned_1 :: N=(N: one=1 two=2) M=(M: 1..10) {
  head -n $N < $in \
    | head -n $M
}

# Defining work plans to get more than just one-off experiments (this is intended primarily for config files)
# ducttape can be called with "ducttape workflow.tape -p Basics" to run cross products of branches by adding lines
# like the following to config files:
plan Basics {
  # 4 experiments/realizations: the branches one and two with with all branches of whichSize
  # The * operator indicates a cross-product
  # The "score" indicates that score is the goal task ("target" in GNU Make lingo)
  # and only dependencies of that task should be run
  reach planned_2 via (WhichSize: smaller) * (N: one two) * (M: 1..10)

  # * 2 experiments/realizations: just the large model (e.g. if it takes much longer to run)
  # * "planned_1 and planned_2" indicates that those 2 tasks are the goal tasks
  #   This is used only to demonstrate syntax. Since planned_1 is a parent of planned_2,
  #   mentioning it has no effect in this case
  reach planned_1, planned_2 via (WhichSize: bigger) * (N: one) * (M: 2 8)

  # It is also possible to use the plan-glob operator '*' as
  # a branch name to indicate "all branches" should be used in the cross-product
  # This will result in the realizations:
  # * WhichSize.bigger (N.one and M.1 are omitted from the name since it is the baseline)
  # * WhichSize.bigger-N.two (M.1 is omitted from the name since it is the baseline)
  reach planned_2 via (WhichSize: bigger) * (N: *) * (M: 1)
}
