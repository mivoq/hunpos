Hunpos is an open source reimplementation of TnT, the well known part-of-speech tagger by Thorsten Brants.

Features
========

* Free and open source, even for commercial use.
* For languages with more complex morphologies, HMM tagging could be quite competitive with the current generation of learning algorithms applying e.g. SVM and CRF methods. A major advantage is that the training/tagging cycle is orders of magnitude faster than in more complex models.
* Precision of tagging on unknown and unseen words was a major priority for us during the development of hunpos.
* Works smoothly with large tag sets. For example in Hungarian, as in other highly inflecting languages, it is important to preserve detailed morphological information in the POS tags in order to provide useful clues for higher level processing tasks. This leads to a significantly larger tagset than is common in English (744 tags here as opposed to the 36 standardly used in Treebank work), but does not degrade training and tagging performance. Although it would make the training process of non-generative models computationally expensive.
* Effortless integration of knowledge from morphological analyzers/dictionaries into best path calculation.
* Contextualized lexical probabilities with a context window of any size. Unlike traditional HMM models, HunPos estimates emission (lexical) probabilities based on the current tag and previous tags as well.
* Hunpos has been implemented in OCaml, a high-level language which supports a succinct, well-maintainable coding style. OCaml has a high-performance compiler that produces native code with speed comparable to C/C++ implementations.

How to install
==============

The suggested building procedure is by use ```cmake``` tool.

On Linux or MacOSX:

* Install cmake
* Compile hunpos
```
mkdir build
cd build
cmake .. -DCMAKE_INSTALL_PREFIX=install
make
make install
```

This will build hunpos binaries, hunpos modules and C binding and install them
in ```build/install```.
With respect to hunpos modules, only hmm_tagger and morphtable interfaces are
installed. If you need to access other interfaces from the installed version,
please update the ```install_ocaml_interfaces``` call in ```CMakeLists.txt```
accordingly and re-run the build procedure.

Notes on building
=================

Traditionally hunpos have been compiled using ```./build.sh``` script, that is
using ocamlbuild to perform compilation of the ```trainer.native``` and
```tagger.native``` binaries. This building strategy is working with most
OCaml versions for Linux, Windows and Mac OS X.

When the C binding have been introduced ```build.sh``` script has not been
updated accordingly and a Makefile has been added. Unfortunately building
the C binding is not cross platform and it conflicts with building the binaries,
so that users have to compile binaries, clean the sources and then compile
the binging.

Neither of these building support out-of-tree compilation, that can be very
useful while building the software for several platforms (using cross
compilation) or with several configurations.

In order to be able to build both binaries and C binding, and solve most of the
above mentioned issues, a new CMake building procedure has been introduced.
The procedure is in its early stage and still requires some work, however it
is currently able to handle:
* building the binaries and the C binding, on Linux (C binding on amd64 architecture is usable only if the OCaml runtime has been compiled with -fPIC, which is not the case with the packaged OCaml compiler of some old linux distributions, such as the current Ubuntu LTS distribution) and Mac OSX (C binding compilation is usable only when compiled with development version of OCaml; see https://github.com/ocaml/ocaml/pull/988 for more information);
* cross-compilation, on Linux targeting Windows (mingw 32bit and 64bit compilers);
* out-of-tree compilation;
* parallel building.

Compilation on Windows has not been tried yet.

