# 1) Running a single command
#
# Syntax:
# * This is just a single command that writes output to stdout and stderr
# * Lines that begin with # are comments
# * Comment blocks preceeding task declarations are associated with that task
#
# Execution:
# * After ducttape has been added to your PATH, this workflow can be executed with:
#   $ ducttape basic.tape
# * Artifacts from this workflow will be in the current directory of basic.tape (./)
# * Both echo commands will be run in the working directory ./hello-world/baseline/work
# * stdout will be placed at ./hello-world/baseline/stdout.txt
# * To have ducttape write a bash script that runs this step to
#      ./hello-world/baseline/commands.sh, use:
#   $ ducttape --dry-run --write-scripts basic.tape
[hello-world]
  echo hi
  echo >&2 hello
