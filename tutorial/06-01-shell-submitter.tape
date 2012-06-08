# Chapter 6: Submitters
# Lesson 1:  Shell
#
# Using the shell submitter is equivalent
# to directly typing each tasks' commands on the command line

task hello_shell {
  echo hello
}

# $COMMANDS are the bash commands from some task
# In this case, the variable will contain "echo hello"
submitter shell :: COMMANDS {
  action run > exit_code {
    eval $COMMANDS
  }
}
