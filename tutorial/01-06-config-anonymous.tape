# Chapter 1: Basics
# Lesson 6:  Anonymous configs
#
# TODO: More documentation

task hello_world :: who=$someone {
  echo hello $who
}

# an anonymous configuration block
# this creates no additional directory structure
config {
  someone=world
}
