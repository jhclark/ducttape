Travis CI Build Status: [![Build Status](https://secure.travis-ci.org/jhclark/ducttape.png?branch=master)](http://travis-ci.org/jhclark/ducttape)

Introduction
============

This is the in-progress baking of Ducttape, a workflow management system for researchers who heart unix. This is a complete overhaul of the LoonyBin workflow manager. Currently, it is alpha quality. Proceed with caution and report bugs regularly.

Want to learn how to use Ducttape? Read [TUTORIAL.md](https://github.com/jhclark/ducttape/blob/master/tutorial/TUTORIAL.md).

Want to help develop ducttape? Read [HACKING.md](https://github.com/jhclark/ducttape/blob/master/xHACKING.md).

Design Principles
=================

* Simple things should remain simple.

* You should be able to start simple. Run. Add complexity. And re-run.

* Workflow management and data flows have very different design patterns than other sorts of code.
  Writing them in C++, Java, or XML is annoying.

Updates
=======

To keep updated on the latest versions of ducttape, subscribe to our low-traffic announcement mailing list: https://groups.google.com/group/ducttape-announce

To stay in the loop on ducttape development, subscribe to our higher traffic development mailing list: https://groups.google.com/group/ducttape-dev

To keep updated on bleeding edge development of ducttape, subscribe to our higher traffic commits mailing list: https://groups.google.com/group/ducttape-commits


Emacs Mode
==========

To get syntax highlighting in emacs, add a line similar to the following in your ~/.emacs file:

```
(load "$PATH_TO_DUCTTAPE_HERE/tool-support/emacs/ducttape.el")
```

Vim Mode
========

To get syntax highlighting in vim, copy (or symlink) the ducttape's vim syntax highlighting file, like so:

```
$ mkdir -p ~/.vim/syntax
$ cp $PATH_TO_DUCTTAPE_HERE/tool-support/vim/ducttape.vim ~/.vim/syntax/ducctape.vim
```

Then add a line to your ~/.vimrc to create an association with .tape files:

```
syntax on
filetype on
au BufRead,BufNewFile *.tape set filetype=ducttape
```
