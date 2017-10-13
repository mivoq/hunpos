ocaml-cmake
===========

CMake Scripts for OCaml

The scripts are in the directory cmake/.

You can find some examples in the directory examples/.

NOTES
=====

These scripts are an adaptation from https://github.com/ocaml-cmake/ocaml-cmake
with the goal of simplifying cross-platform compilation of mixed OCaml and C
code.

They have only been tested in a single project in order to produce a shared
library that can be used from C. On the other hand compilation is been
tested through continuos integration on a Debian-based amd64 system using
the packaged compiler and the packaged mingw32 and mingw64 cross-compilers,
as well as the opam-cross-windows cross compilers, on Mac OSX using development
version of ocaml, on Windows using opam-repository-mingw and Visual Studio 2015.
As of today compilation on Debian-based amd64 using the packaged mingw32 and
mingw64 cross-compilers produces unusable executables with some configuration.

On Mac OSX it is not possible to use any current OCaml stable release
(i.e., <= 4.05) to build usable shared libraries; for this reason the
development version is the only supported version on Mac OSX if shared
libraries are needed.

The scripts are not bug-free and not complete yet and will probably require
some changes to suit most needs. Feel free to contribute back your changes.
