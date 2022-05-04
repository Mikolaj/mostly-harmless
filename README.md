# mostly-harmless
This is a discussion, draft and notebook repo for largely benign automatic differentiation experiments, in particular using library https://github.com/Mikolaj/horde-ad.

To compile portions of this repo you need a local checkout of `horde-ad` at `../` and a `cabal.project.local` file that contains the line `packages: ../horde-ad`.

Some executable tools in this repo:

* `cabal test ad-test --test-show-details=direct` --- this runs tests (various toy examples and moderate size fully connected neural networks) with library `ad` and verifies that the results are close to those of `horde-ad`. The expected results encoded in the test file come from `horde-ad` so there's lots of error messages that usually show tiny difference coming from floating point error.

* `cabal bench prod-ad` --- library `ad` benchmark of computing the gradient of a huge product of parameters. A similar benchmark exists for library `backprop` (see below) and for `horde-ad` in its repo.

* `cabal bench mnist-ad` --- library `ad` benchmark of training and testing a fully connected neural network recognizing MNIST digits, defined via operations on scalars stored in a vector. Note that this is not a fair benchmark, for many reasons. Standard neural networks should be defined using matrix operations to have any chance of efficiency. Even if implemented using scalar operations, they should use unboxed vectors, which `ad` doesn't support.

* `cabal bench prod-backprop` --- library `backprop` benchmark of computing the gradient of a huge product of parameters.

* `cabal bench mnist-backprop` --- library `backprop` benchmark (taken from the `backprop` source code, see https://github.com/Mikolaj/mostly-harmless/blob/master/CREDITS.md) running MNIST, as for `ad` and `horde-ad`, but using matrix operations (from `hmatrix`) and deducing gradients from dozens of them, instead of millions of scalar operations; two variants `bp-lens` and `bp-hkd` are included and also a benchmark  using a fully handwritten gradient.

* `cabal bench prod-all` --- running the huge product benchmark using three handwritten gradients (one of them from library `backprop`) and using a gradient derived from scalar operations by `horde-ad`, `ad` and `backprop`. See a chart at https://combinatronics.com/Mikolaj/mostly-harmless/b9f1ed13b7e549f7f221f2ea4a1033ce59677247/misc-bench/ProdAll-78ba46b.html (but these results may be outdated by the time you read this).

* `cabal bench mnist-all` --- running the MNIST benchmark using a gradient derived from scalar operations by `ad` and derived from scalar, vector and matrix operations by `horde-ad` and derived from matrix operations by `backprop` using two different APIs and also a completely handwritten gradient. See a chart at https://combinatronics.com/Mikolaj/mostly-harmless/bf1014389f69a143bba1525560b9d0b09f0f1a31/misc-bench/MnistAll-efd12c3.html (but these results may be outdated by the time you read this).
