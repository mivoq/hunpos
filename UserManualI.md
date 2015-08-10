UserManual
Hunpos is easy to use command line tool for part-of-speech tagging

With a precompiled binary and a model in place, simply type

./hunpos-tag en\_wsj.model

This will process standard input to standard output. The input must be one token per line. Empty lines are sentence separators. The output copies the input, adding the predicted part-of-speech tag to the end of each line, separated by a TAB. Training

With a training corpus at hand, you can easily train your own part-of-speech model. The expected training corpus format is the same as the output format of the tagger.

{{cat uzbek.corpus | ./hunpos-train uzbek.model}}}

builds the uzbek.model based on the corpus. The model file is not human-readable, but for OCaml hackers we note that it is a standard binary serialization of the model data structure.
Options

There is currently just one option for tagging.

-g N :

> Suffix-based guessing gives P(tag|token) probabilities, but for the HMM, we need P(token|tag) probabilities, so Bayes' Theorem is used (see Brants 2000 for details). But even before this "Bayesian inversion" step, we throw away the tags with the lowest probabilities. The number of tags to keep can be set by the -g option. The default value is 10, and the default is suitable for most applications. TnT can be very slow when there is a sequence of 3 or more unseen words in a sentence if the size of the tag set is large. This problem is solved by -g parameter. If you have <100 different tags forget this parameter.

There are of course many other parameters of the algorithm. These currently can not be set by command line options. Such is for example the parameter-set of the Viterbi beam search.
Training options

You can set several parameters for training. The two most important parameters control HMM order.

-t N :

> The order of the tag transition probability is set by the -t option. This tells the system to estimate the probability of a tag based on the previous k tags. The default is 2. P(t\_3 | t\_1,t\_2)

-e N :

> The order of the emission probability is set by the -e option. This tells the system to estimate the probability of a token based on the previous k tags. (Including the tag of the token itself.) The default is 2. P(w\_2 | t\_1,t\_2)

The "-t 3 -e 1" setup fully emulates what TnT does, but for some languages and applications, "-t 3 -e 2" may be favourable. Unseen words

-f N :

> The algorithm estimates an unseen word's tag distribution based on the tag distribution of rare words. 'Rare' is defined as seen less than N times in the train corpus. This rareness parameter N can be set by the -f option. Its default value is 10.

-s N :

> The -s parameter sets the length of the longest suffix to be considered by the algorithm when it estimates an unseen word's tag distribution. The default value is 10. The optimal value of this parameter may depend on the morphology and orthography of the language considered.

BUILD

For off-the-shelf use, we recommend the pre-compiled binaries available for all major platforms. To build from source code, the ocaml compiler (version 3.10) must be present on your system.

After downloading and extracting the source, simply type:

./build.sh build

You will find the trainer.native and tagger.native executables in the build/hunpos directory.
AUTHORS

The HunPos? code was written by Péter Halácsy (peter@halacsy.com). Conceptual and technical contributors: András Kornai, Csaba Oravecz, Dániel Varga.
Licence

CC Attribution