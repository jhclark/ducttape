# * "checkout" is run in a sandbox directory
# * All other commands are run inside $dir
versioner disk :: path {
  action checkout > dir {
    if [ ! -e $path ]; then
      echo >&2 "Directory does not exist: $path"
      exit 1
    fi
    ln -s $path'/'* $dir/
  }
  action repo_version > version {
    if [ ! -e $path ]; then
      echo >&2 "Directory does not exist: $path"
      exit 1
    fi
    echo "VERSIONING_UNSUPPORTED" > $version
  }
  action local_version > version date {
    if [ ! -e $path ]; then
      echo >&2 "Directory does not exist: $path"
      exit 1
    fi
    echo "VERSIONING_UNSUPPORTED" > $version
    echo "VERSIONING_UNSUPPORTED" > $date
  }
}
