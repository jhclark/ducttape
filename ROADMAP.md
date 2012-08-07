V0.1
====

* Initial version

V0.2
====

* First public release

Mainstream features:

* Branch points
* Branch grafting
* Sequence branches
* Config files
* Packages
* Detects when new versions are available
* Realization plans: easily enumerate which experimental variations you wish to run, summarize, or invalidate
* Task invalidation: specify a set of branches that should be re-run
* CLI globbing: Use wildcards to specify which realizations should be run, invalidated, etc.
* Globs in realization plans
* Detection of failed tasks (non-zero exit code; did all files get created?)
* Sandboxing of each task and realization
* Ability to extend a workflow that has already been run without redoing unnecessary work
* Package version tracking

Unsupported features:

* Submitters (current syntax is deprecated)
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


* Advanced submitter state tracking: is my job queued or running? (Planned for future version)
* Import statement (Planned for future version)
* "Functions"