# COMMANDS: the bash commands from some task
submitter shell :: COMMANDS {
  action run {
    eval "$COMMANDS"
  }
}
