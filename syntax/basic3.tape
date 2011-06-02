# This is a command that takes 2 input files (a and b)
# and produces no output
[devnull] : x y
  cat $x>/dev/null
  cat $y>/dev/null
