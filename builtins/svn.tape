# * "checkout" is run in a sandbox directory
# * All other commands are run inside $dir
versioner svn :: repo rev {
  action checkout > dir {
    svn co -r $rev $repo $dir
  }
  action repo_version > version {
    svn info $repo | awk -F': ' '/Revision/{print $2}' > $version
#    svn info $repo | awk -F': ' '/Date/{print}' > $date
  }
  # Used to confirm version after update/install
  action local_version > version date {  
    svn info | awk -F': ' '/Revision/{print $2}' > $version
    svn info | awk -F': ' '/Date/{print}' > $date
  }
}