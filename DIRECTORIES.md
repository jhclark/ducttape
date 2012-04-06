Appendix A: Directory Structure

Base Directory
==============

$BASE is the directory in which your workflow.tape file sits. This may become configurable at some point.

Packages
========

Packages get build in $BASE/.packages/$VERSION/$PACKAGE_NAME

The Attic
=========

Partial output or invalidated output from previous runs gets moved to $BASE/.attic/$TASK_NAME/$REALIZATON_NAME/$VERSION. This may help prevent accidental deletion of data and can help diagnose issues if tasks fail repeatedly.

"Flat" Structure
================

Iff the "structure flat;" directive is at the top of your ducttape file, tasks will be stored in $BASE/$TASK_NAME. We refer to this directory as $TASK.

The bash code in your task block will start off with its cwd as $TASK and its output files including ducttape_stdout.txt, ducttape_exit_code.txt, and ducttape_version.txt, ducttape_submitter.sh will be written there. If you re-run the workflow, any partial output may be moved to The Attic (see above).

If you later choose to switch to "structure hyper;", you will need to run a conversion utility that will *attempt* to automatically insert the additional directory structure for a hyperworkflow (see below). It will attempt to detect any hardcoded absolute paths for you, but *hardcoded absolute paths in your outputs files must be changed manually to ensure that future runs work as expected.*

"Hyper" Structure
=================

If no directive or the "structure hyper;" directive is at the top of your ducttape file, tasks will be stored in $BASE/$TASK_NAME/$REALIZATION_NAME. We refer to this directory as $TASK.

Just as in the flat structure, the bash code in your task block will start off with its cwd as $TASK and its output files including ducttape_stdout.txt, ducttape_exit_code.txt, and ducttape_version.txt, ducttape_submitter.sh will be written there. If you re-run the workflow, any partial output may be moved to The Attic (see above).

Config Directories
==================

A basic workflow without any configs (the configuration blocks using the "config" keyword") or an anonymous config such as "config {}" will have the the structure defined by flat or hyper above. However, each config is housed in its own top-level directory under which all tasks live.

For the "flat" structure, this makes the $TASK directory: $BASE/$CONF_NAME/$TASK_NAME

For the "hyper" structure, this makes the $TASK directory: $BASE/$CONF_NAME/$TASK_NAME/$REALIZATION_NAME

For example, if we have a "config experimentA {}"... 

Development note: to allow the anonymous config to output tasks directly in $BASE, the config assignments in an anonymous config must be specific to the anonymous config, not part of a global namespace. Do we now need a global config to handle this? Or do we disallow hierarchical namespaces like that altogether? Or do we allow both of "config {}" and "config _ {}" or some other string mashing?