# Using package versioning
#
# * During R&D, software often changes while a workflow is running
# * To reproduce a workflow, you need to know what version of
#   the software you ran
# * in, out, and N are shown only to illustrate syntax
[param-step] codeheap < in=/etc/passwd > out :: N=5
  ls | $codeheap/??? | tee $out

# TODO: Need to show some simple build
# TODO: Remove reference to jhclark
# * Build commands are only called when versioner indicates a new version
[PACKAGE:codeheap] @versioner=git @url=/home/jhclark/
  # We don't actually need to do this for python code,
  # but for the sake of example...
  python -m compileall .

#############################################################################
# The following implementation of a git versioner is actually built-in and
# automatically available to all workflows -- it is provided here for clarity
#############################################################################

# * "install" is run in a sandbox directory
# * All other commands are run inside $dir
[VERSIONER:git] :: url rev
  +checkout > $dir
    git clone $url $dir
  +update
    git pull
    git checkout $rev ???
  +head-version > version
    git ls-remote origin HEAD | cut -f1 > $version
  +cur-version > version date             # Used to confirm version after update/install
    git rev-parse HEAD > $version
    git log -1 | awk '/^Date/{$1=""; print}' > $date
