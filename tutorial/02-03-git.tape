# Chapter 2: Packages
# Lesson 3:  Understanding the git versioner
#
# * During R&D, software often changes while a workflow is running
# * To reproduce a workflow, you need to know what version of
#   the software you ran
# * in, out, and N are shown only to illustrate syntax

# note: the package syntax is currently experimental since minor changes are planned for a future release.
global {
  ducttape_experimental_packages=enable
}

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
# * repo_version should return an *absolute* revision identifier. This identifier should include any branch information,
#   if relevant.
# * local_version should return current *absolute* revision relative to the repository from 
#   which the tool was checked out. This version should *never* be a relative, mutable
#   revision identifier such as HEAD or TRUNK.
versioner git :: repo ref {
  action checkout > dir {
    git clone $repo $dir
  }
  action repo_version > version {
    git ls-remote $repo $ref | cut -f1 > $version
  }
  # Used to confirm version after checkout
  action local_version > version date {
    git rev-parse HEAD > $version
    git log -1 | awk '/^Date/{$1=""; print}' > $date
  }
}
