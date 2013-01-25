task feeder < in=(TestSet: x=/dev/null y=/dev/null) > out {}

task works1 < in=$out@feeder[TestSet:*] {}

global {
  file=(TestSet: x=/dev/null y=/dev/null)
}
task works2 < in=$file[TestSet:*] {}

global {
  var=$out@feeder
}
task broken < in=$var[TestSet:*] {}
