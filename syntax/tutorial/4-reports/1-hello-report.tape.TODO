# TODO: Unfinished tutorial step

# Reports enable you to run commands after a workflow has
# been (partially) completed and emit key-value pairs 
# for use in an ASCII or Latex table or even a plot.
#
# You can give reports names to create reports
# for debugging, status checking, or publication.

# what about a simple step with only a report?  
[g2.simpleReport] a > x
  cat $a > $x
  +report reportA
    echo "Line1=$(fgrep error $x)"

