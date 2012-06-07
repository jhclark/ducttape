# Chapter 1: Basics
# Lesson 7:  Named configs
#
# Named configs allow... TODO

task hello_named_world :: who=$someone {
  echo hello $who
}

# an named configuration block
# all tasks will be nested under an additional subdirectory called "World"
config World {
  someone=world
}

global {
  ducttape_structure=flat
}
