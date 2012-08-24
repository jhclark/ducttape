# Chapter 1: Basics
# Lesson 8:  External config files
#
# External config files allow you to separate variables (and even tasks)
# that don't belong in a more generic workflow.
#
# For example, if you wish to build a workflow that builds a large system
# based on data, it's possible to separate the main building workflow
# from any references to specific data files.

task hello_external_world :: who=$someone {
  echo hello $who
}

global {
  ducttape_structure=flat
}
