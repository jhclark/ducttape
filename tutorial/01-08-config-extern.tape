# Chapter 1: Basics
# Lesson 8:  External config files
#
# TODO: Why are these useful?

task hello_external_world :: who=$someone {
  echo hello $who
}

global {
  ducttape_structure=flat
}
