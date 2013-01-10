#!/bin/bash

# Let's see if we can see the internet
wget --no-check-certificate -O /dev/null www.github.com &> /dev/null

if [ "$?" -eq "0" ]; then
    ducttape 02-03-git.tape "$@"
else
    echo "Not running 02-03-git.tape, because it requires an active connection to the internet"
fi
