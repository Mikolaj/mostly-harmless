# mostly-harmless
This is a discussion, draft and notebook repo for largely benign automatic differentiation experiments, in particular using the library https://hackage.haskell.org/package/horde-ad.

Preliminary benchmark results, with GHC 9.10.2. Once GHC 9.14 is out, this will be re-benchmarked and horde-ad will be optimized using the results.

* `cabal bench prod-all` --- running the huge product benchmark using three handwritten gradients (one of them from library `backprop`) and using various flavours of automatic differentiation of `horde-ad`, `ad` and `backprop`: https://rawcdn.githack.com/Mikolaj/mostly-harmless/14fd6f5173335470756c88aab40e909df6c40953/misc-bench/ProdAll-initial-GHC9.10.2.html

* `cabal bench mnist-all` --- running the MNIST benchmark using various flavours of automatic differentiation of `horde-ad`, `ad` and `backprop`, the latter also with a completely handwritten gradient: https://rawcdn.githack.com/Mikolaj/mostly-harmless/14fd6f5173335470756c88aab40e909df6c40953/misc-bench/MnistAll-initial-GHC9.10.2.html


## Old content, including benchmark results for 2022 horde-ad

Some executable tools in this repo:

* `cabal test ad-test --test-show-details=direct` --- this runs tests (various toy examples and moderate size fully connected neural networks) with library `ad` and verifies that the results are close to those of `horde-ad`. The expected results encoded in the test file come from `horde-ad` so there's lots of error messages that usually show tiny difference coming from floating point error.

* `cabal bench prod-ad` --- library `ad` benchmark of computing the gradient of a huge product of parameters. A similar benchmark exists for library `backprop` (see below) and for `horde-ad` in its repo.

* `cabal bench mnist-ad` --- library `ad` benchmark of training and testing a fully connected neural network recognizing MNIST digits, defined via operations on scalars stored in a vector. Note that this is not a fair benchmark, for many reasons. Standard neural networks should be defined using matrix operations to have any chance of efficiency. Even if implemented using scalar operations, they should use unboxed vectors, which `ad` doesn't support.

* `cabal bench prod-backprop` --- library `backprop` benchmark of computing the gradient of a huge product of parameters.

* `cabal bench mnist-backprop` --- library `backprop` benchmark (taken from the `backprop` source code, see https://github.com/Mikolaj/mostly-harmless/blob/master/CREDITS.md) running MNIST, as for `ad` and `horde-ad`, but using matrix operations (from `hmatrix`) and deducing gradients from dozens of them, instead of millions of scalar operations; two variants `bp-lens` and `bp-hkd` are included and also a benchmark  using a fully handwritten gradient.

* `cabal bench prod-all` --- running the huge product benchmark using three handwritten gradients (one of them from library `backprop`) and using a gradient derived from scalar operations by `horde-ad`, `ad` and `backprop`. See a chart at https://rawcdn.githack.com/Mikolaj/mostly-harmless/83a09e1162b7e34aab343c6d079c64123544234a/misc-bench/ProdAll-78ba46b.html (but these results may be outdated by the time you read this).

* `cabal bench mnist-all` --- running the MNIST benchmark using a gradient derived from scalar operations by `ad` and derived from scalar, vector and matrix operations by `horde-ad` and derived from matrix operations by `backprop` using two different APIs and also a completely handwritten gradient. See a chart at https://rawcdn.githack.com/Mikolaj/mostly-harmless/83a09e1162b7e34aab343c6d079c64123544234a/misc-bench/MnistAll-efd12c3.html (but these results may be outdated by the time you read this).
