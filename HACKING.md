Some instructions for those wishing to contribute to ducttape development:

Git
===

To get a local copy of ducttape:

```bash
git clone git:github.com/jhclark/ducttape.git
```

Eclipse
=======

First, download the correct version of Eclipse (3.6 Helios seems to fair better than 3.7 Indigo as of April 2012) and the version of the Scala IDE plug-in for Scala 2.9. The newest version of Eclipse isn't always supported, so you might end up with multiple versions of Eclipse on your machine. See http://scala-ide.org/download/current.html. We recommend using a fresh Eclipse directory (without any other plug-ins) and a fresh workspace for your Scala projects.

Using Git from the Command Line
-------------------------------

```bash
mkdir workspace # The directory to be used by your Scala Eclipse
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

Using Git from Inside Eclipse
-----------------------------

Now import the project into the workspace from GitHub. Right click in the Package Explorer and select Import. In the Import dialog, select Git -> Projects from Git. Next, select URI. In the Source Git Repository tab, fill out the following:

* Under Location -> URI, type https://$USER@github.com/jhclark/ducttape.git. Where $USER is your github username.  The Host and Repository path fields will be automatically filled out for you.
* Protocol is https 
* Don't change the user or password

Finally, select Import existing projects.


Console
=======

If you wish to develop from the command line, you should use either the emacs or vi syntax highlighting mode that is distributed with Scala.


Scala Style
===========

We recommend reading this guide as a starting place for Scala stype: http://davetron5000.github.com/scala-style/

Ducttape code should wrap after no more than 120 characters. If you can't fit more than 80 characters wide on your screen,
please buy a new monitor. Generally, functions should fit within about 40-50 lines (aka, a screenfull). Shorter is usually better.
There's plenty of ducttape code that breaks this rule. When possible, fix.


Scaladoc
========

Source code should be commented using ScalaDoc standards. See https://wiki.scala-lang.org/display/SW/Scaladoc.