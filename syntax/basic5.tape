# Now what if you have a parameter that's not a file?
# List these parameters after a colon in your task declaration
[param-step] > out : N=5
  echo "The parameter N is $N"
  echo $N > $out
