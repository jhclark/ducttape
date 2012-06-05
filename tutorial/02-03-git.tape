# Chapter 2: Packages
# Lesson 3:  Understanding the git versioner
#
# * During R&D, software often changes while a workflow is running
# * To reproduce a workflow, you need to know what version of
#   the software you ran
# * in, out, and N are shown only to illustrate syntax
task lunchtime : lunchpy {
  $lunchpy/lunch.py Indian Mexican Italian
}

# * Build commands are only called when versioner indicates a new version
package lunchpy :: .versioner=git .repo="git://github.com/mjdenkowski/lunchpy.git" .ref=HEAD {
  # We don't actually need to compile anything for python code,
  # but for the sake of example we'll make this program run a bit faster
  python -m compileall .
}

#############################################################################
# The following implementation of a git versioner is actually built-in and
# automatically available to all workflows -- it is provided here for clarity
#############################################################################

# * Checkout is run in a sanboxed directory and $dir will be a subdirectory (makes using git easier)
# * All other commands are run inside $dir
# * As we will see with inline branches, specializations such as checkout and update
#   inherit all of their parent's parameters so that update has visibility of $repo and $rev
versioner git :: repo ref {
  action checkout > dir {
    git clone $repo $dir
  }
  action repo_version > version {
    git ls-remote $repo $ref | cut -f1 > $version
  }
  # TODO: Can we do without this? Just check repo version as we checkout? (potential race condition)
  # Used to confirm version after checkout
  action local_version > version date {
    git rev-parse HEAD > $version
    git log -1 | awk '/^Date/{$1=""; print}' > $date
  }
}
