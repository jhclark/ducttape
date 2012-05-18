task preproc :: p1=(DataSet: train=big.txt test=small.txt) {
   echo ${p1}
}

task tok :: p2=$p1@preproc {
   echo ${p2}
}

task trainer :: p3=$p1@preproc[DataSet:train] {
   echo ${p3}
}
