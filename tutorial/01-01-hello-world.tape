#!/usr/bin/env ducttape

# Chapter 1: Basics
# Lesson 1:  Running a single command

# Syntax:
# * This is just a single command that writes output to stdout and stderr
# * Lines that begin with # are comments
# * Comment blocks preceeding task declarations are associated with that task
#
# Execution:
# * After ducttape has been added to your PATH, this workflow can be executed with:
#   $ ducttape basic.tape
# * Artifacts from this workflow will be in the current directory of basic.tape (./)
# * This task will run in the working directory ./hello-world/baseline/1/work
# * stdout will be placed at ./hello-world/baseline/1/stdout.txt
# * To have ducttape write a bash script that runs this step to
#      ./hello-world/baseline/commands.sh, use:
#   $ ducttape --dry-run --write-scripts basic.tape
task hello_world {
  echo hi
  echo >&2 hello
}

# The ducttape_structure global is a special directive to ducttape (ignore it for now)
global {
  ducttape_structure=flat
}
