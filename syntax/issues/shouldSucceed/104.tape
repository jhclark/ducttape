# At t2, we expect $in1 to contain "1" and $in2 to contain "2" (but in this bug, instead both files contain "1")
task t1 :: p=(P: one=1 two=2) > out { echo $p > $out }
task t2 < in1=$out@t1[P:one] in2=$out@t1[P:two] { cat $in1 $in2 }
