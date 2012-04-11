# COMMANDS: the bash commands from some task
submitter shell :: COMMANDS {
  action run {
    $COMMANDS
  }
}

task hello {
  echo hello
}
