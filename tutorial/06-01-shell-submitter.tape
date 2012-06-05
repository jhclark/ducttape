# Chapter 6: Submitters
# Lesson 1:  Shell
#
# Using the shell submitter is equivalent
# to directly typing each tasks' commands on the command line

task hello {
  echo hello
}

submitter sge :: COMMANDS /* the bash commands from some task */ {
  action run > exit_code {
    $COMMANDS
  }
}
