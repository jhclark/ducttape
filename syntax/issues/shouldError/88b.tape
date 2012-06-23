task t1 :: p=(BP1: branch1=1 branch2=2) > out {
  echo $p > $out
}

# We would like to error here for now
# Notice that branch1 and branch2 are being swapped
task t2 < in=(BP1: branch1=$out@t1[BP1:branch2] branch2=$out@t1[BP1:branch1]) {
  cat $in
}
