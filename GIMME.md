-----
Gimme
-----

Gimme specifies a standardized command line interface for programs that can exactly
reproduce a repository-checkout-and-build within the context of a larger workflow system.
This can become important in computational research in which software updates and experiments
rapidly iterate with many code updates happening between experiments. For the sake of
reproducibility, it is important to know and be able to reproduce *exactly* the
software configuration used in each experiment.

The Interface
=============

gimme
-----

Syntax:
```bash
./my_builder gimme $repo $dest [--rev $rev] [--branch $branch]
```

This command requests that the builder "my_builder" retrieve its target tool from the
repository specified by $repo, build any binary components and place them at $dest.
If $rev is omitted, it is taken to be the latest version; otherwise, it is interpreted[
as a revision relative to $repo. If $branch is omitted, it is taken to be HEAD (git)
or TRUNK (svn); otherwise, it should be specified as a branch known to $repo.

For example:
```bash
./my_builder.sh gimme https://github.com/jhclark/multeval.git /tmp/multeval-012345 HEAD
```

version
-------

Syntax:
```bash
./my_builder version $dest
```

For example:
```bash
./my_builder.sh version /tmp/multeval-012345
```
might return the git SHA1 revision:
```
7425f75
```
