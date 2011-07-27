Introduction
============

This is the in-progress baking of DuctTape, a workflow management system for researchers who heart unix. This is a complete overhaul of the LoonyBin workflow manager. Currently, it is not yet suitable for production use.

Building
========

Assuming you have Scala 2.9.0+ installed, just run:

```bash
./build.sh
```

This will download the required version of scala test and build a JAR file using the Scala Compiler.

Testing
=======

To test low level data structures, run:

```bash
./test-unit.sh
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
