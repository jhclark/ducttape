# TODO: Unfinished tutorial step

# Globbing realizations...
# $x will be a space-delimited string
[g2.globRealization] x=$g2.doStuff/a!whichThing(*) > y
  for file in $x; do echo $file; done
