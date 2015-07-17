# TISBL for C99

This is a TISBL interpreter written in C99.  It comes with a driver program that
provides both interactive and non-interactive modes.  The language
implementation is designed and written as a library and may be used in any
program.


## System requirements

A compliant C99 compiler.  No non-standard functions or features are used.  The
GNU function `getopt_long` is implemented by the bundled `getopt.*` files.


## To get this to run

To build the default driver program on a Unix-like system, simply run `make`.
It will build the library and driver binaries.

The driver program supports both interactive and non-interactive modes.  See
`tisbl --help` for details.

