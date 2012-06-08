# Chapter 1: Basics
# Lesson 3:  Reading input files
#
# * This task takes 2 input files (a and b) and produces no output
# * Ducttape will set the environment variables in $a and $b before invoking bash
task hello_world_3 < a=/etc/passwd b=/etc/hosts {
  echo "I will be reading files called $a and $b and doing nothing useful with them"
  cat $a>/dev/null
  cat $b>/dev/null
}

global {
  ducttape_structure=flat
}
