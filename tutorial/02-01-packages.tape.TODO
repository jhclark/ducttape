# Chapter 2: Packages
# Lesson 1:  Using package versioning
#
# * During R&D, software often changes while a workflow is running
# * To reproduce a workflow, you need to know what version of
#   the software you ran
# * in, out, and N are shown only to illustrate syntax
task lunchtime_with_git <- lunchpy {
  $lunchpy/lunch.py Indian Mexican Italian
}

# TODO: Move this to a separate tutorial lesson
# What if we want to run with a specific ref
# (in git, this could mean a specific revision or branch)
task lunchtime_with_git_ref <- lunchpy(ref="d4053f5") {
  $lunchpy/lunch.py Indian Mexican Italian
}

# TODO: Move this to a separate tutorial lesson
# What if we want to build lunchpy with specific compile options
# As a trivial example, we'll use compileall -l (do not recurse)
# But in practice, you might want to compile using various experimental libraries statically linked in
# or compile in certain time- or space-optimized data structures
#
# Notice that we can specify package options right next to versioner options
task lunchtime_with_compile_opt <- lunchpy(ref=HEAD, opts="-l") {
  $lunchpy/lunch.py Indian Mexican Italian
}

# * Build commands are only called when versioner indicates a new version
# The following versioners are built-in to ducttape and implemented under $DUCTTAPE/builtins:
# * git
# * svn
#
# Note: We don't actually need to compile anything for python code,
# but for the sake of example we'll make this program run a bit faster
package lunchpy(opts="") from git(repo="git://github.com/mjdenkowski/lunchpy.git", ref=HEAD) {
  python -m compileall "$opts" .
}

# The idea here is to reproduce a repository-checkout-and-build within the context of a larger workflow system.
# This can become important in computational research in which software updates and experiments
# rapidly iterate with many code updates happening between experiments. For the sake of
#reproducibility, it is important to know and be able to reproduce *exactly* the
# software configuration used in each experiment.
