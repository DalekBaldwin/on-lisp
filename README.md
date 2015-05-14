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

An online HTML version of the book, minus graphical figures, can be found [here](http://dunsmor.com/lisp/onlisp/onlisp.html).

This code has been tested on SBCL, CCL, CLISP, and ECL. The simplest way to get it up and running is to install [Quicklisp](https://www.quicklisp.org) and clone this repository into your `quicklisp/local-projects` directory.
