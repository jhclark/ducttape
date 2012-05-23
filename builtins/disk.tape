# * "checkout" is run in a sandbox directory
# * All other commands are run inside $dir
versioner disk :: path {
  action checkout > dir {
    ln -s $path/* $dir/
  }
  action repo_version > version {
    echo "VERSIONING_UNSUPPORTED" > $version
  }
  action local_version > version date {
    echo "VERSIONING_UNSUPPORTED" > $version
    echo "VERSIONING_UNSUPPORTED" > $date
  }
}
