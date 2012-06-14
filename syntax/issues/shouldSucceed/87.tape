global {
  global1=1
  global2=2
}

# We expect 2 realizations here, not one
task t :: p=(BP1: branch1=$global1[BP1:branch1] branch2=$global2[BP1:branch2]) {}
