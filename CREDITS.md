Credits go to

Conal Elliott, talking beautifully about the Essence of Automatic Differentiation

Simon Peyton Jones, who sketched in "Provably correct, asymptotically efficient, higher-order reverse-mode automatic differentiation" talk at https://www.youtube.com/watch?v=EPGqzkEZWyw algorithms that this Haskell codebase implements

Faustyna Krawiec, Neel Krishnaswami, Tom Ellis, Andrew Fitzgibbon, Richard Eisenberg, the remaining authors of the paper "Provably correct, asymptotically efficient, higher-order reverse-mode automatic differentiation" from POPL 2022 that describes an AD translation of a functional code (allegedly, eventually to C++), including some ingenious algorithmic ideas this Haskell codebase implements

Oleg Grenrus, whose https://hackage.haskell.org/package/overloaded contain inspiring examples related to AD

Edward Kmett, author of the archetype Haskell https://github.com/ekmett/ad library

Justin Le, author of the https://github.com/mstksg/backprop library and the amazing articles at https://blog.jle.im



Authors of file https://github.com/microsoft/knossos-ksc/blob/93d454592705a2847d58db5e1f741710bfcf076d/users/awf/prod.hs, copied into this repo as `misc-bench/ProdManualTools.hs` and modified, copyright (c) Microsoft Corporation, licensed under the MIT license.

Authors of files `t10k-images-idx3-ubyte.gz`, `t10k-labels-idx1-ubyte.gz`, `train-images-idx3-ubyte.gz`, `train-labels-idx1-ubyte.gz` from http://yann.lecun.com/exdb/mnist, copied into this repo into directory `samplesData/`, copyright (according to https://keras.io/api/datasets/mnist) Yann LeCun and Corinna Cortes for the MNIST dataset, which is a derivative work from original NIST datasets, copyright by their authors and made available under the terms of the Creative Commons Attribution-Share Alike 3.0 license.

Justin Le, again, who authored the file https://github.com/mstksg/backprop/blob/master/bench/bench.hs, copied into this repo as `backprop-bench/MnistBackpropTools.hs` and modified, copyright owned by Justin Le, released under the BSD3 license.
