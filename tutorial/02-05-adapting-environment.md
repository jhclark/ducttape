Adapting for Your Environment
=============================

Currently, system specific modifications such as where prefix, etc is located is recommended to be done by copying the build scripts. This may seem bad, but build system and repository system already abstract fairly complicated operations. Each package should generally contain small scripts, which act more or less like configuration files.

However, it is considered best practice to have variables for such things on each package block rather than buried in the commands. For example, you should make it easy to modify the number of cores used in the build for make -j, scons -j, bjam -j, etc.
