# TODO: Copy boilerplate from git
# Use this only if git and svn are completely foreign
# concepts to you. Beyond just getting started with ducttape,
# use of this versioner is discouraged.

# TODO: Use proper rsync args
# TODO: Disk versioner cannot support updating to a particular revision...
[VERSIONER:disk] :: url rev
  +checkout > $dir
    rsync $url $dir
  +update
    rsync $url $dir
  +head-version > version
    find $url -type f -printf '%TY-%Tm-%Td_%TT %p\n' | sort | awk 'END{print $1}' > $version
  +cur-version > version date
    find . -type f -printf '%TY-%Tm-%Td_%TT %p\n' | sort | awk 'END{print $1}' > $date
    cp $date $version
