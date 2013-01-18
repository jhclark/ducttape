# Chapter 7: Future Plans
# Lesson 2:  Functions

package my_tool :: .versioner=disk .path=/ {}

# To define a "task function" in the same file
# Functions may not declare submitters -- the mapping onto resources is separated out.
# TODO: Functions will eventually support "default" arguments for parameters such as p
#       Such parameters may either be overridden or omitted at call sites {
func reusable : my_tool < in > out :: p {
  cat < $in > $out
  echo "p=$p"
}

# Using that function or subworkflow later in the file:
# (Require naming inputs and outputs again for clarity, but commands are ommitted)
task x calls reusable < in=small.txt > out :: p="something" ;

# Parameters may also be overridden at the call site
task y calls reusable < in=big.txt > out :: p="something else" ;

# Note: Adding or renaming packages is not allowed
#task bad calls reusable : my_tool < in=small.txt > out
