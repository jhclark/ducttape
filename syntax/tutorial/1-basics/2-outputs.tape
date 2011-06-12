# 2) Writing output files
#
# * Ducttape will assign the paths for the output files
#   as something like ./hello-world-2/x and ./hello-world-2/y.txt
# * The environment variables x and y.txt will be set by
#   ducttape before calling bash
# * Note that bash requires the use of ${} for using variables containing .
[hello-world-2] > x y.txt
  echo writing files $x and $y...
  echo hello > $x
  echo world > ${y.txt}
