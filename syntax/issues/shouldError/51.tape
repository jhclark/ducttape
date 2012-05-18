task hello_world > who
     		 :: who=$someone
		 :: what=1
		 :: what=2 {
  echo hello $who
}

config {
  // redefine global
  someone=hello
  someone=world
}
