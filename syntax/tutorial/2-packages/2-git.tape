# Using package versioning
#
# * During R&D, software often changes while a workflow is running
# * To reproduce a workflow, you need to know what version of
#   the software you ran
# * in, out, and N are shown only to illustrate syntax
task param_step : codeheap < in=/etc/passwd > out :: N=5 {
  ls | $codeheap/do_stuff | tee $out
}

# * Build commands are only called when versioner indicates a new version
package codeheap :: .versioner=git .url=/home/codeheap_git_repo {
  # We don't actually need to do this for python code,
  # but for the sake of example...
  python -m compileall .
}

#############################################################################
# The following implementation of a git versioner is actually built-in and
# automatically available to all workflows -- it is provided here for clarity
#############################################################################

# * Checkout is run in a sanboxed directory and $dir will be a subdirectory (makes using git easier)
# * All other commands are run inside $dir
# * As we will see with inline branches, specializations such as checkout and update
#   inherit all of their parent's parameters so that update has visibility of $url and $rev
versioner git :: url rev {
  action checkout > dir {
    git clone $url $dir
  }
  action update {
    git pull
    git checkout $rev
  }
  action repo_version > version {
    git ls-remote origin HEAD | cut -f1 > $version
  }
  # Used to confirm version after update/install
  action local_version > version date {  
    git rev-parse HEAD > $version
    git log -1 | awk '/^Date/{$1=""; print}' > $date
  }
}