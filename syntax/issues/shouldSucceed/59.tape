task hello {
	echo hello
}

task world :: planet=(Planet: Mercury Venus Earth Mars) {
	echo ${planet}
} 

plan {
	reach hello, world
}
