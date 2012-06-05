submitter sge :: COMMANDS /* the bash commands from some task */ {
  action run > exit_code {
    $COMMANDS
  }
}

task hello {
  echo hello
}
