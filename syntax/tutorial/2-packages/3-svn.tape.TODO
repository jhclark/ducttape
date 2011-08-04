# 6) Using package versioning with SVN
[use-svn] cmdutils < in=/etc/passwd > out :: N=5
  loonybin -version

[PACKAGE:loonybin] @versioner=svn @url=http://loonybin.svn.sourceforge.net/svnroot/loonybin
  ant

#############################################################################
# The following implementation of a git versioner is actually built-in and
# automatically available to all workflows -- it is provided here for clarity
#############################################################################

# * "install" is run in a sandbox directory
# * All other commands are run inside $dir
[VERSIONER:svn] :: url rev
  +checkout > $dir
    svn co -r $rev $url .
  +update
    svn up -r $rev
  +head-version > version
    svn info $url | awk -F': ' '/Revision/{print $2}' > $version
    svn info $url | awk -F': ' '/Date/{print}' > $date
  +cur-version > version date             # Used to confirm version after update/install
    svn info | awk -F': ' '/Revision/{print $2}' > $version
    svn info | awk -F': ' '/Date/{print}' > $date
