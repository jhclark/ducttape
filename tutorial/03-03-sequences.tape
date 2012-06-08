# Chapter 3: HyperWorkflows
# Lesson 3:  Sequence Branch Points
#
# Branches can also be created based on re-running the task multiple times
# * This can be useful when the underlying task makes calls to a random number generator (e.g. optimizers)
# * The step receives the integer ID of this trial as a parameter
# * Ducttape handles the assignment of sequential ID numbers to whichTrial by the 1..10 construction
task run_several_times > x :: trial=(WhichTrial: 1..10) {
  # Use bash's built-in random number generator
  echo $RANDOM > $x
}
