# * "checkout" is run in a sandbox directory
# * All other commands are run inside $dir
versioner git :: repo ref {
  action checkout > dir {
    git clone --progress $repo $dir
  }
  action repo_version > version {
    git ls-remote $repo $ref | cut -f1 > $version
  }
  # TODO: Can we do without this? Just check repo version as we checkout? (potential race condition)
  # Used to confirm version after checkout
  action local_version > version date {
    git rev-parse HEAD > $version
    git log -1 | awk '/^Date/{$1=""; print}' > $date
  }
}
