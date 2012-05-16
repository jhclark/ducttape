task preproc > in=(DataSet: train=big.txt test=small.txt) {
   wc -l ${in}
}

task trainer < in=$in@preproc {
   wc -l ${in}
}
