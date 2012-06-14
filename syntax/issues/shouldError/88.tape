global {
  global1=1
  global2=2
}

# We would like to error here for now
# Notice that branch1 and branch2 are being swapped
task t :: p=(BP1: branch1=$global1[BP1:branch2] branch2=$global2[BP1:branch1]) {}
