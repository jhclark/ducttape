task t1 :: p=(BP1: branch1=1 branch2=2) > out { cat $p > $out }
task t2 < in=(BP2: branch3=$out@t1[BP1:branch1] branch4=$out@t1[BP1:branch2]) { cat $in }
