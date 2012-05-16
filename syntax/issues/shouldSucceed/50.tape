task preproc :: in=(DataSet: train=big.txt test=small.txt) {
   wc -l ${in}
}

task trainer :: in=$in@preproc[DataSet:train] {
   wc -l ${in}
}
