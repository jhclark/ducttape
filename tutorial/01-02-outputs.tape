

# Chapter 1: Basics
# Lesson 2:  Writing output files
#
# * Ducttape will assign the paths for the output files
#   as something like ./hello-world-2/x and ./hello-world-2/y.txt
# * The environment variables x and y_txt will be set by
#   ducttape before calling bash
# * Note that bash disallows variables containing .
task hello_world_2 > x y_txt {
  echo writing files $x and $y_txt...
  echo hello > $x
  echo world > $y_txt
}

# Since variables with dots are not allowed by bash (and therefore ducttape),
# ducttape allows you to specify the output file name
# This is useful so that:
# * types can still be automatically detected based on extension
# * it's easy to refer to outputs of programs with hardcoded output names (e.g. unix split)
task named_output < x=/etc/passwd > named=x.gz {
  cat $x | gzip > $named
}

global {
  ducttape_structure=flat
}
