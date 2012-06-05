#!/usr/bin/env bash
set -e
set -o pipefail
set -u

workflow=$1

dir=$(dirname $workflow)
file=$(basename $workflow)

db=$dir/.$file.cache.db
# Check if sqlite DB is older than the workflow file
# If so, call ducttape to update things

## TODO


sqlite $db 'create table tasks (name TEXT PRIMARY KEY);'
sqlite $db 'insert into tasks (name) values ("ohai");'
sqlite $db 'select * from tasks;'
