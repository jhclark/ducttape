# 4) Running tasks with dependencies

# First a step we already know about...
[first] > x
  for i in {1..10}; do
    echo $i >> $x
  done

# * We use first's output "x" as the input "a" by using the = operator
# * Instead of specifying an absolute path as before, we specify
#   it as a dependency using the "$" prefix
# TODO: $first/x should become $x@first
[and_then] < a=$first/x > x
  cat < $a > $x
