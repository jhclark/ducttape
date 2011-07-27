# TODO: Unfinished tutorial step

# Packing can also be done based on re-running the task multiple times
# * This can be useful when the underlying task makes calls to a random number generator (e.g. optimizers)
# * The step receives the integer ID of this trial as a parameter
# * Ducttape handles the assignment of sequential ID numbers to whichTrial by the 1..10 construction
[g2.runSeveralTimes] > x :: trial=(whichTrial: 1..10)
  echo $RANDOM > $x
