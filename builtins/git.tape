# * "checkout" is run in a sandbox directory
# * All other commands are run inside $dir
versioner git :: repo ref {
  action checkout > dir {
    git clone --progress $repo $dir
    # In case the repo has submodules, grab those, too
    cd $dir
    git submodule update --init --recursive
  }
  # This is perhaps more accurately called the "expected" version
  action repo_version > version {
    if [[ "$ref" == "HEAD" ]]; then
      # ls-remote may return several lines, so we grep for the one we're
      # actually interested in.
      # Example: There may be multiple HEADs, the one of the local (queried)
      # repository and the one of a remote repository.
      git ls-remote $repo $ref | grep -E "^[0-9a-f]+[^A-Za-z0-9]+$ref\$" | cut -f1 > $version
    else
      echo "$ref" > $version
    fi
  }
  # TODO: Can we do without this? Just check repo version as we checkout? (potential race condition)
  # Used to confirm version after checkout
  action local_version > version date {
    git rev-parse HEAD > $version
    git log -1 | awk '/^Date/{$1=""; print}' > $date
  }
}
