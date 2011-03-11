#!/usr/bin/env bash

egrep -o '^ITERATION [0-9]+' vest.log | tail -n 1
egrep -ho '^DECODER SCORE: [0-9.]+' vest.log | tail -n 2
egrep -ho '^PROJECTED SCORE: [0-9.]+' vest.log | tail -n 1
fgrep 'OPT-ITERATION' vest.log | tail -n 1

if [[ "$1" == "-v" ]]; then
    cd vest-work

    echo "Weights:"
    ls -t weights.* | egrep -v '[0-9+]-[0-9]+$' | head -n3
    ls -t weights.* | egrep -v '[0-9+]-[0-9]+$' | head -n3 | xargs paste | cat -n

    n=5
    echo "Reference #1:"
    head -n$n ../separate-reference-files/ref0 | cat -n

    echo "Output:"
    ls -t run.raw.*.gz | head -n 2
    for file in $(ls -t run.raw.*.gz | head -n 2); do
	zcat $file | cat -n | head -n$n
	echo
    done
fi