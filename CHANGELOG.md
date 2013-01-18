v0.3-pre2: Jan 18, 2013
=======================

* Depth-first traversal of workflow to encourage fail-fast behavior (breadth-first also available)
* Fixed bug that was throwing errors about the .versions directory -- workflow version information is now stored properly
* 'versions' mode added
* Support for ~ in filenames
* A script called ducttape_task.sh is now written in reach task's output directory to help debugging -- ducttape does not actually run this script
* Function blocks are now supported (see tutorial/07-02-functions.tape)
* More concise output

Coming soon:
* Graft globbing
* Groups

v0.3-pre1: Jan 16, 2013
=======================

* Add directive for ducttape_auto_update_packages=disable along with 'update' mode to allow updating packages only on demand
* Added "tabular" script for use with summary mode
* Fixed bug in summary mode that caused table rows to be unnecessarily separated
* Improvements to 'viz' mode including packed, unpacked, and debug visualizations
* Print full realization names next to canonical directory names during execution confirmation
* Allow multiple plans to be specified at once on the command line
* Allow reach...via lines clauses in plans to contain newlines
* Plenty of minor bug fixes

v0.2.1: Aug 24, 2012
===================

* Minor bug fixes

v0.2: Aug 7, 2012
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
* Execute tasks in parallel: "ducttape -j12" in the spirit of GNU Make, etc.
* Summaries: quickly make tables from many experimental variations

* Package version tracking: know exactly what revision(s) produced the output of your workflow -- bundled with integration for SVN and git
* New package version notification: detects when new package versions are available

Unsupported features:

* Packages: explicitly enumerate (current syntax is deprecated)
* Submitters: automatically submit tasks to a job scheduler on a cluster -- bundled with support for Torque and Sun Grid Engine (current syntax is deprecated)
* Bash CLI completion (partially working)
* Detection of unused and undeclared bash variables (currently this is too aggressive)
* Web UI
* Run multiple instances at the same time via file-based locks (not yet reliable)
* Visualization of workflows via GraphViz


v0.1: April 2012
================

* Initial version
