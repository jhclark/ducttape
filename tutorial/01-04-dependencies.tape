# Chapter 1: Basics
# Lesson 4:  Running tasks with dependencies

# First a step we already know about...
task first > x {
  for i in {1..10}; do
    echo $i >> $x
  done
}

# * We use first's output "x" as the input "a" by using the = operator
# * Instead of specifying an absolute path as before, we specify
#   it as a dependency using the "$" prefix
task and_then < a=$x@first > x {
  cat < $a > $x
}

global {
  ducttape_structure=flat
}
