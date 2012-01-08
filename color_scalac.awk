#!/usr/bin/awk -f

BEGIN { FS=": " }

{
  if($2=="error") {
    print "\033[91m"$0
  } else {
    print "\033[0m"$0
  }
}
