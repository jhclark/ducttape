# 6) Using package versioning with SVN
task experiment_task : moses < in=/etc/passwd > out :: N=5 {
  $moses -version
}

package moses :: .versioner=svn .url="http://moses.svn.sourceforge.net/svnroot/moses" .rev=HEAD {
  ./configure
  make -j16
}

# TODO: Eventually, ducttape will support "bundling" a workflow to be shared and reproduced
#       by other researchers. This package versioning system allows us to just include
#       the workflow configuration and SVN/git repo URLs and build commands. Others
#       can then reproduce our experiments just by customizing the workflow for
#       their machine's resources.
# TODO: Given a workflow bundle and the --reproduce option, ducttape should
#       checkout and build exactly the version of the code indicated by the bundle
#       for each task. It can accomplish this by overriding the .rev parameter
#       with the value required for reproduction rather than HEAD or some user choice.

#############################################################################
# The following implementation of a git versioner is actually built-in and
# automatically available to all workflows -- it is provided here for clarity
#############################################################################

# * "checkout" is run in a sandbox directory
# * All other commands are run inside $dir
versioner svn :: url rev {
  action checkout > dir {
    svn co -r $rev $url .
  }
  action update {
    svn up -r $rev
  }
  action repo_version > version {
    svn info $url | awk -F': ' '/Revision/{print $2}' > $version
    svn info $url | awk -F': ' '/Date/{print}' > $date
  }
  # Used to confirm version after update/install
  action local_version > version date {  
    svn info | awk -F': ' '/Revision/{print $2}' > $version
    svn info | awk -F': ' '/Date/{print}' > $date
  }
}