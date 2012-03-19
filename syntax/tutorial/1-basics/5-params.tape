# 5) Using parameters
#
# * Parameters are for variables that aren't files
# * They are listed after inputs and outputs, using a double colon
# * Because we distinguish files from parameters, ducttape can check if input
#   files exist before running length commands or submitting jobs to a scheduler
[param-step] < in=/etc/passwd > out :: N=5 {
  echo "$in has $(wc -l < $in) lines"
  echo "The parameter N is $N"
  echo $N > $out
}

# * The distinction between files and parameters also means
#   that parameters don't introduce temporal dependencies when
#   they are references (like this step)
# * "no-dep" can start running in parallel with "param-step"
[no-dep] :: X=$N@param-step
  echo "X=$N"
}
