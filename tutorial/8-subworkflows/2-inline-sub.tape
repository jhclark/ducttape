# To define a subworkflow in the same file:
group tm < in1=x.txt > out1=$out@y {
  task x < in=$in1@tm > out {
    cat < $in >$out
  }
  task y < in=$out@x > out {
    cat < $in > $out
  }
}

# To define a "task function" in the same file
func filter < in > out {
  cat < $in > $out
}

# Using that function or subworkflow later in the file:
# (Require naming inputs and outputs again for clarity, but omit commands)
task x = filter < in=x.txt > out
