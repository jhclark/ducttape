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
