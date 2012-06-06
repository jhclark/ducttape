Appendix A: Directory Structure
===============================

Destination Directory
=====================

$DEST is by default the $PWD from which you run ducttape This may become configurable at some point.

You can also define inside your config the value "ducttape.dest_dir=/my/directory" as the destination directory.

Packages
========

Packages get built in $DEST/ducttape.packages/$PACKAGE_NAME/$PACKAGE_VERSION

The Attic
=========

Partial output or invalidated output from previous runs gets moved to $DEST/ducttape.attic/$TASK_NAME/$REALIZATON_NAME/$WORKFLOW_VERSION. This may help prevent accidental deletion of data and can help diagnose issues if tasks fail repeatedly. The workflow version is incremented every time the workflow is executed.

"Flat" Structure
================

Iff the "ducttape.structure=flat" directive is in your configuration, tasks will be stored in $DEST/$TASK_NAME. We refer to this directory as $TASK.

The bash code in your task block will start off with its cwd as $TASK and its output files including ducttape_stdout.txt, ducttape_exit_code.txt, and ducttape_version.txt, ducttape_submitter.sh will be written there. If you re-run the workflow, any partial output may be moved to The Attic (see above).

If you later choose to switch to "ducttape.structure=hyper", you will need to follow the special instructions below.

"Hyper" Structure
=================

If you do not specify the structure or "ducttape.structure=hyper" is in your configuration, tasks will be stored in $DEST/$TASK_NAME/$REALIZATION_NAME. We refer to this directory as $TASK.

Just as in the flat structure, the bash code in your task block will start off with its cwd as $TASK and its output files including ducttape_stdout.txt, ducttape_exit_code.txt, and ducttape_version.txt, ducttape_submitter.sh will be written there. If you re-run the workflow, any partial output may be moved to The Attic (see above).

Config Directories
==================

A basic workflow without any configs (the configuration blocks using the "config" keyword") or an anonymous config such as "config {}" will have the the structure defined by flat or hyper above. However, each config is housed in its own top-level directory under which all tasks live.

For the "flat" structure, this makes the $TASK directory: $DEST/$CONF_NAME/$TASK_NAME

For the "hyper" structure, this makes the $TASK directory: $DEST/$CONF_NAME/$TASK_NAME/$REALIZATION_NAME

Transitioning from "Flat" to "Hyper"
====================================

If you started with a flat structure and want to use hyper, ducttape will detect that you changed your structure preference
and warn you that the directory structures are incompatible. You'll need to add a special transitional directory as the
destination of hyperworkflow tasks, so set "ducttape.transitional_dir=/my/director/hyperdir". We will refer to this directory
as $TRANS. Your original flat tasks won't be moved, but all new tasks will be placed in $TRANS.

For the new hyper structure, this makes the $TASK directory: $TRANS/$TASK_NAME/$REALIZATION_NAME
Or if a conf name is specified: $TRANS/$CONF_NAME/$TASK_NAME/$REALIZATION_NAME

Your original flat tasks will be symlinked into $TRANS as: $TRANS/$TASK_NAME/baseline -> $DEST/$TASK_NAME, since each flat task represents the baseline branch of the Baseline branch point in a hyperworkflow.
