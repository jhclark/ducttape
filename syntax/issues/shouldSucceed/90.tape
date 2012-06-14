task t1 :: p=(BP1: branch1=1 branch2=2) { echo $p }
task t2 :: p=(BP2: branch3=$p@t1[BP1:branch1] branch4=$p@t1[BP1:branch2]) { echo $p }
