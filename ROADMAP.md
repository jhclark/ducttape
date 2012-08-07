V0.1: April 2012
================

* Initial version

V0.2: Aug 7, 2012?
==================

* First public release

Mainstream features:

* Branch points
* Branch grafting
* Sequence branches
* Config files: specify multiple sets of inputs in separate files while maintaining a core workflow in a central place
* Realization plans: easily enumerate which experimental variations you wish to run, summarize, or invalidate
* Task invalidation: specify a set of branches that should be re-run
* CLI globbing: Use wildcards to specify which realizations should be run, invalidated, etc.
* Globs in realization plans
* Detection of failed tasks: checks for non-zero exit code and all specified outputs being created
* Ability to extend a workflow that has already been run without redoing unnecessary work

* Package version tracking: know exactly what revision(s) produced the output of your workflow -- bundled with integration for SVN and git
* New package version notification: detects when new package versions are available

Unsupported features:

* Packages: explicitly enumerate (current syntax is deprecated)
* Submitters: automatically submit tasks to a job scheduler on a cluster -- bundled with support for Torque and Sun Grid Engine (current syntax is deprecated)
* Summaries: quickly make tables from many experimental variations
* Bash CLI completion (partially working)
* Detection of unused and undeclared bash variables (currently this is too aggressive)
* Web UI
* Run multiple instances at the same time via file-based locks (not yet reliable)

Not implemented:

* Checksumming files that changed outside of ducttape's control (Planned for V0.3)
* Checksumming of ducttape files
* Invalidation of incompatible versions?
* Advanced branch points? (Planned for V0.4)
* Branch globbing (planned for V0.5)
* Reproducible workflow export

* Advanced submitter state tracking: is my job queued or running? (Planned for future version)
* Import statement (Planned for future version)
* "Functions"