# TODO: Unfinished tutorial step

# Sometimes it is useful to have a single task that has access
# to multiple branches; for example, when evaluating statistical
# significance of several simulations as compared to a baseline.

task g2_unpack < a=$a@g2_doStuff[whichThing:choiceA] > x {
   cat < $a > $x
}
