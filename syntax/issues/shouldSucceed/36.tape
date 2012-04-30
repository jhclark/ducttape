submitter bad :: foo COMMANDS {
  action run {
    wrapper="ducttape_job.sh"
    echo "echo foo=${foo}" >> $wrapper
    echo "$COMMANDS" >> $wrapper
    bash $wrapper
  }
}

task hello_bad :: .submitter=bad .foo=bar {
  echo bad
}

submitter good :: COMMANDS {
  action run {
    wrapper="ducttape_job.sh"
    echo "$COMMANDS" >> $wrapper
    bash $wrapper
  }
}

task hello_good :: .submitter=good {
  echo good
}