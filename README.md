Introduction
============

This is the in-progress baking of DuctTape, a workflow management system for researchers who heart unix. This is a complete overhaul of the LoonyBin workflow manager. Currently, it is not yet suitable for production use.

Updates
=======

To keep updated on the latest versions of ducttape, subscribe to our low-traffic announcement mailing list: https://groups.google.com/group/ducttape-announce

Building
========

TODO: Move this section to a separate file once we move out of alpha...

Assuming you have Scala 2.9.0+ installed, just run:

```bash
sbt compile
```

To continuously recompile whenever source files change:
```bash
sbt ~compile
```

This will download the required version of scala test and build a JAR file using the Scala Compiler.

Documentation
=============

To generate the tutorial documentation as doc/doc.pdf:
```bash
./doc.sh
```


To generate scaladoc:
```bash
sbt doc
```

Testing
=======

To test low level data structures, run:

```bash
sbt test
```

To test high-level functionality, run:

```
./test-regression.sh
```
This will run all of the example files associated with the tutorial, which also serve as regression tests.


Eclipse
=======

To develop using Eclipse:

```
mkdir workspace
cd workspace
git clone git:github.com/jhclark/ducttape.git
mkdir ducttape/lib
cp /path/to/scalatest-1.6.1.jar ducttape/lib
```

Now, open Eclipse using the workspace you just created.
Perform File..Import..General..Existing Projects into Workspace.
Select workspace as the root directory of the project to import, make sure that the ducttape project is selected, then click Finish.
Eclipse should now compile the code. Once Eclipse has completed building the workspace, exit Eclipse.
Re-open Eclipse, and you should be good to go. To verify, open ducttape.scala in the scala directory, and select Run As..Scala Application.

Setting Up Connections to Remote Machines (Not Yet Implemented)
===============================================================

# You will constantly have a SSH tunnel active to the remote machine
```
local.com$ ssh -L3333:localhost:4444 -N remote.com
```

# Start the remote server on the remote machine (e.g. a cluster head node)
```
remote.com$ scala ducttape.remote.RemoteServer 4444
```

# Now the remote client can control what happens on the remote machines
```
local.com$ scala ducttape.remote.RemoteClient 3333
```
