# This is a command that takes 2 input files (a and b)
# and produces no output
[devnull] a b
  echo "I will be reading files called $a and $b"
  cat $a>/dev/null
  cat $b>/dev/null
