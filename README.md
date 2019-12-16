[![Build Status](https://travis-ci.org/DalekBaldwin/on-lisp.svg?branch=master)](https://travis-ci.org/DalekBaldwin/on-lisp)

This repository contains a version of the code from On Lisp modified for use in modern Lisp environments. Paul Graham's original code can be found [here](http://ep.yimg.com/ty/cdn/paulgraham/onlisp.lisp).

Among other necessary changes, this version:

- Updates code that relied on pre-ANSI built-ins
- Includes the bug fixes mentioned on pg's [errata page](http://www.paulgraham.com/onlisperrata.html)
- Organizes everything into a modern system structure with ASDF and named-readtables
- Adapts most of the example code into test suites
- Makes it easy to load each version of the query system, Prolog system, and OOP system separately

It was written to follow along with the book page by page and catalogue dependencies between the chapters, which become quite complex toward the end.

The PDF file of the book available from Paul Graham's site isn't so great for reading on a screen. A version with smaller margins and the missing figures re-added can be found [here](http://www.lurklurk.org/onlisp/onlisp.html).

An online HTML version of the book, minus graphical figures, can be found [here](http://lisp.esthlos.com/onlisp/on_lisp.html).

This code has been tested on SBCL, CCL, CLISP, and ECL. The simplest way to get it up and running is to install [Quicklisp](https://www.quicklisp.org) and clone this repository into your `quicklisp/local-projects` directory.

Beginner's GNU/Linux quickguide, for those just starting with Lisp:

Have one of the tested Lisps installed on your system ( SBCL, CCL, CLISP or ECL ). Rlwrap is also suggested for ease of console use, but not required [available here]( https://github.com/hanslub42/rlwrap )
Install quicklisp and make sure it loads every time by following instructions in the installer or web-page  `( ql:add-to-init-file )`
By default, quicklisp installs to your home directory. For example, in terminal execute: 

`cd ~/quicklisp/local-projects/`

`git clone https://github.com/DalekBaldwin/on-lisp/`

`sbcl `

`(ql:quickload "on-lisp")`


To test that Your Lisp flavor is compatible with the On Lisp code, run the included on-lisp-test. It will download required libraries if not installed on the system, that might take some time. Execute the following in your REPL to run the test

`(ql:quickload "on-lisp-test")`


