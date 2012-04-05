task foo > out=a.txt {
	echo "hello" > ${out}
}

task bar < in=$out@foo {
	cat $in
}
