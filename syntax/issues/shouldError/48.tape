task first > out {
   echo foo > ${out}
}

task second :: a=$out@first {
   cat ${a}
}
