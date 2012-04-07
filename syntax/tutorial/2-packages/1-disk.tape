# 1) Using package versioning without any source code control / versioning system
# Use this only if git and svn are completely foreign
# concepts to you. Beyond just getting started with ducttape,
# use of this versioner is discouraged.

task experiment_task : moses < in=/etc/passwd > out :: N=5 {
  $moses -version
}

package moses :: .versioner=disk .url="http://moses.svn.sourceforge.net/svnroot/moses" .rev=NULL {
  ./configure
  make -j16
}

# TODO: Use proper rsync args
# TODO: Disk versioner cannot support updating to a particular revision...
versioner disk :: url rev {
  action checkout > dir {
    rsync $url $dir
  }
  action repo_version > version {
    find $url -type f -printf '%TY-%Tm-%Td_%TT %p\n' | sort | awk 'END{print $1}' > $version
  }
  action local_version > version date {
    find . -type f -printf '%TY-%Tm-%Td_%TT %p\n' | sort | awk 'END{print $1}' > $date
    cp $date $version
  }
}