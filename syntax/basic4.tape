# 4) Running tasks with dependencies

# First a step we already know about...
[first] > x
  seq 10 > $x

# * We use first's output "x" as the input "a" by using the = operator
# * Instead of specifying an absolute path as before, we specify
#   it as a dependency using the "$" prefix
[and-then] a=$first/x > x
  cat < $a > $x
