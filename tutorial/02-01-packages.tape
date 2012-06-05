# Chapter 2: Packages
# Lesson 1:  Using package versioning
#
# * During R&D, software often changes while a workflow is running
# * To reproduce a workflow, you need to know what version of
#   the software you ran
# * in, out, and N are shown only to illustrate syntax
task lunchtime : lunchpy {
  $lunchpy/lunch.py Indian Mexican Italian
}

# * Build commands are only called when versioner indicates a new version
# The following versioners are built-in to ducttape and implemented under $DUCTTAPE/builtins:
# * git
# * svn
package lunchpy :: .versioner=git .repo="git://github.com/mjdenkowski/lunchpy.git" .ref=HEAD {
  # We don't actually need to compile anything for python code,
  # but for the sake of example we'll make this program run a bit faster
  python -m compileall .
}

# The idea here is to reproduce a repository-checkout-and-build within the context of a larger workflow system.
# This can become important in computational research in which software updates and experiments
# rapidly iterate with many code updates happening between experiments. For the sake of
#reproducibility, it is important to know and be able to reproduce *exactly* the
# software configuration used in each experiment.
