#!/usr/bin/awk -f

BEGIN { FS=": " }

{
  if($2=="error") {
    # see http://stackoverflow.com/questions/287871/print-in-terminal-with-colors-using-python
    printf "\033[94m"$1
    for(i=2; i<=NF; i++) printf ": \033[91m"$i
    printf "\n"
  } else {
    print "\033[0m"$0
  }
}
