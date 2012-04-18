Some instructions for those wishing to contribute to ducttape development:

Eclipse
=======

First, download the correct version of Eclipse (3.6 Helios seems to fair better than 3.7 Indigo as of April 2012) and the version of the Scala IDE plug-in for Scala 2.9. The newest version of Eclipse isn't always supported, so you might end up with multiple versions of Eclipse on your machine. See http://scala-ide.org/download/current.html.

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