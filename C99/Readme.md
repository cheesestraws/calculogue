# TISBL for C99

This is an embeddable TISBL interpreter written in C99.  It comes with an
example driver program that provides both interactive and non-interactive modes,
but the language implementation is designed and written as a library and may be
used in any program.


## System requirements

A compliant C99 compiler.  No non-standard functions or features are used.  The
GNU function `getopt_long` used by the driver program is provided by
[getopt\_port](https://github.com/kimgr/getopt_port) by Kim Gräsman in the
bundled `getopt.*` files.


## To get this to run

To build the default driver program on a Unix-like system, run `make`.  It will
build both the library and driver binaries.  To build it with the IDE of your
choice, click on things until it works.

The driver program supports both interactive and non-interactive modes.  See
`tisbl --help` for details.


## Things to note

The current implementation relies heavily on the libc heap and is thus far
slower than it could be given the language.

C99 compound and designated literals turned out to be more useful during
prototyping than the final implementation, as object construction moved into
functions that perform additional initialization.

C99 fixed size integer types came in handy for packing values and locations.

