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
./my_builder gimme $dest
```

This command requests that the builder "my_builder" retrieve its target tool from the
repository specified by $repo, build any binary components and place them at $dest.
If $rev is omitted, it is taken to be the latest version; otherwise, it is interpreted[
as a revision relative to $repo. If $branch is omitted, it is taken to be HEAD (git)
or TRUNK (svn); otherwise, it should be specified as a branch known to $repo.

This command should also always output a file called gimme.ver in the $dest dir, which
stores the result of "my_builder version $dest". Importantly, this will return an
*absolute* revision identifier. This identifier should include any branch information,
if relevant.

The address 

Future versions may require "gimme" to provide a specific reversion and branch as arguments.
For now, if such reproducibility is desired, it should be accomplished by directly modifying
the gimme builder script "my_builder".

For example:
```bash
./my_builder.sh gimme /tmp/multeval-012345
```

dest-version
------------

Syntax:
```bash
./my_builder dest-version $dest
```

This command returns the current *absolute* revision relative to the repository from 
which the tool was checked out. This version should *never* be a relative, mutable
revision identifier such as HEAD or TRUNK.

For example:
```bash
./my_builder.sh dest-version /tmp/multeval-012345
```
might return the git SHA1 revision:
```
7425f75
```

repo-version
------------

```bash
./my_builder repo-version https://github.com/jhclark/multeval.git
```


Adapting for Your Environment
=============================

Currently, system specific modifications such as where prefix, etc is located is recommended to be done by copying the build scripts. This may seem bad, but build system and repository system already abstract fairly complicated operations. Between the Gimme interface and the build system and repo interfaces, these should be small scripts, which act more or less like configuration files.

However, it is considered best practice to have variables for such things at the top of your gimme script rather than buried in the commands.

TODO
====
Passing in number of cores for make -j, scons -j, bjam -j, etc
