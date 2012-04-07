Basics
======

* Invalidate node and all of its children (mark incomplete), but show what will be killed and ask for confirmation
* Specify goal vertex
* Specify goal realizations
* Check that all output files were created successfully (and store MD5sum?)
* Kill renegate jobs launched to the scheduler? We should really detect whether or not to resume...
* Throw an error if all definitions of the same branch point don"t contain the same branch names

* Allow using inputs as variables without implying temporal ordering
* Why don't unbound inputs fail on workflow compilation?
* Throw error on relative paths in config file? Or resolve them. Right now, they silently cause terrible things to happen
* Define semantics of recursive config variables
* Throw proper errors for poorly formed config variables
* What do we do about branch points that refer to config variables that have branch points?

Submitters
==========

* Dump stderr from job on failure
* Save a compact, whitespace/comment-insensitive format of each taskdef
  under each task to detect if it has changed and might need to be re-run
* Would we like to give options about how realizations are batched within jobs? (packing)
* Show status of jobs w.r.t. schedulers? (Can this be farmed out to shell scripts?)

Grammar
=======

Things that will work as soon as the grammar is done
----------------------------------------------------

* Remove need for spaces around ( and )
* Allow multi-line task headers
* Allow comments at the end of the file?
* Spaces between lines in bash?
* Comments in bash code
* Scheduler inferface: Add syntax for defining how to submit tasks to a scheduler (syntax/tutorial/6-schedulers/2-sge.tape.TODO)
* SCM Interface: Add syntax for defining how to pull code from git/svn along with the repo version (syntax/tutorial/2-packages/3-svn.tape.TODO)
* Code Build Interface: Add syntax for defining build commands for code (syntax/tutorial/2-packages/3-svn.tape.TODO)

Things that require additional coding in addition to the grammar
----------------------------------------------------------------

* Defining "plans" or "realization sets" (syntax/tutorial/3-hyper/3-cross-product.tape.TODO)
* Defining "reports" (syntax/tutorial/4-reports/1-hello-report.tape.TODO)
* "Globbing" branches (syntax/tutorial/3-hyper/7-glob-branch.tape.TODO)
* "Grabbing" branches (syntax/tutorial/3-hyper/6-grab-branch.tape.TODO)
* "Alt" notation for defining different sets of commands for different realizations (syntax/tutorial/3-hyper/5-alts.tape.TODO)

* Allow config file to populate unbound variables? ...if they end in _?















Setting Up Connections to Remote Machines (Not Yet Implemented)
===============================================================

You will constantly have a SSH tunnel active to the remote machine
```
local.com$ ssh -L3333:localhost:4444 -N remote.com
```

Start the remote server on the remote machine (e.g. a cluster head node)
```
remote.com$ scala ducttape.remote.RemoteServer 4444
```

Now the remote client can control what happens on the remote machines
```
local.com$ scala ducttape.remote.RemoteClient 3333
```
