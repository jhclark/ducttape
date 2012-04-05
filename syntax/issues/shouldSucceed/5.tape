config {
	sink=/dev/null
	cpu=/proc/cpuinfo
}

task foo < in=$cpu {
	cat $in > $sink
}
