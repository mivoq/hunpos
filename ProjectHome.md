Hunpos is an open source reimplementation of TnT, the well known part-of-speech tagger by Thorsten Brants.

# Features #

  * Free and open source, even for commercial use.

  * For languages with more complex morphologies, HMM tagging could be quite competitive with the current generation of learning algorithms applying e.g. SVM and CRF methods. A major advantage is that the training/tagging cycle is orders of magnitude faster than in more complex models.

  * Precision of tagging on unknown and unseen words was a major priority for us during the development of hunpos.

  * Works smoothly with large tag sets. For example in Hungarian, as in other highly inflecting languages, it is important to preserve detailed morphological information in the POS tags in order to provide useful clues for higher level processing tasks. This leads to a significantly larger tagset than is common in English (744 tags here as opposed to the 36 standardly used in Treebank work), but does not degrade training and tagging performance.  Although it would make the training process of non-generative models computationally expensive.

  * Effortless integration of knowledge from morphological analyzers/dictionaries into best path calculation.

  * Contextualized lexical probabilities with a context window of any size. Unlike traditional HMM models, HunPos estimates emission (lexical) probabilities based on the current tag and previous tags as well.

  * Hunpos has been implemented in OCaml, a high-level language which supports a succinct, well-maintainable coding style. OCaml has a high-performance compiler that produces native code with speed comparable to C/C++ implementations.
