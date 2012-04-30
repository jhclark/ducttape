global {
	var=(X: x1=1 x2=2)
}

task hello :: x=$var[X:x1] {
	echo $x
}
