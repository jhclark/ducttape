# This is a command that produces 2 output files
#
# The path of the output files is determined by ducttape
# and will be in a sandboxed directory, accessible by head-2
# This pre-determined path will be substituted for $x and $y
# NOTE: String substitution or as environment variables?
[fiat] x y
  echo hello > $x
  echo world > $y
