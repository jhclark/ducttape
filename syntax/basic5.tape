# Now what if you have a parameter that's not a file?
# List these parameters after a colon in your task declaration
# Because we distinguish files from parameters, ducttape can check if input
# files exist before running length commands or submitting jobs to a scheduler
[param-step] > out : N=5
  echo "The parameter N is $N"
  echo $N > $out
