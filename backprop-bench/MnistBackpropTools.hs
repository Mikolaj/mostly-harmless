-- Copyright   : (c) Justin Le 2018-2021
-- License     : BSD3
--
-- Taken from
-- https://github.com/mstksg/backprop/blob/master/src/Numeric/Backprop.hs
-- and modified as below.
{-# LANGUAGE BangPatterns, DataKinds, DeriveGeneric, FlexibleInstances, GADTs,
             LambdaCase, PolyKinds, ScopedTypeVariables, StandaloneDeriving,
             TemplateHaskell, TypeApplications, TypeFamilies, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module MnistBackpropTools where

import Prelude

import Control.DeepSeq
import Control.Monad (foldM)
import Criterion.Main
import Data.Char
import Data.Functor.Identity
import Data.List (foldl', sortOn)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Vector qualified as V
import Data.Vector.Storable qualified
import GHC.Generics (Generic)
import GHC.TypeLits
import Lens.Micro
import Lens.Micro.TH
import Numeric.Backprop
import Numeric.Backprop.Class
import Numeric.LinearAlgebra qualified as HM
import Numeric.LinearAlgebra.Static
import System.Random
import System.Random.MWC qualified as MWC

-- Good enough for QuickCheck, so good enough for me.
shuffle :: RandomGen g => g -> [a] -> [a]
shuffle g l =
  let rnds = randoms g :: [Int]
  in map fst $ sortOn snd $ zip l rnds

type family HKD f a where
    HKD Identity a = a
    HKD f        a = f a

data Layer' i o f =
    Layer { _lWeights :: !(HKD f (L o i))
          , _lBiases  :: !(HKD f (R o))
          }
  deriving (Generic)

type Layer i o = Layer' i o Identity

deriving instance (KnownNat i, KnownNat o) => Show (Layer i o)
instance NFData (Layer i o)

makeLenses ''Layer'

data Network' i h1 h2 o f =
    Net { _nLayer1 :: !(HKD f (Layer i  h1))
        , _nLayer2 :: !(HKD f (Layer h1 h2))
        , _nLayer3 :: !(HKD f (Layer h2 o ))
        }
  deriving (Generic)

type Network i h1 h2 o = Network' i h1 h2 o Identity

deriving instance (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o) => Show (Network i h1 h2 o)
instance NFData (Network i h1 h2 o)

makeLenses ''Network'

backpropBgroupStorable :: [( Data.Vector.Storable.Vector Double
                           , Data.Vector.Storable.Vector Double )]
                       -> Int
                       -> Benchmark
backpropBgroupStorable test n =
  let f (glyphs, labels) =
        ( fromJust $ create glyphs
        , fromJust $ create labels )
  in backpropBgroup (map f test) n

backpropBgroup :: [(R 784, R 10)] -> Int -> Benchmark
backpropBgroup test n =
    env (do
      g <- MWC.initialize
           . V.fromList
           . map (fromIntegral . ord)
           $ "hello world"
      net <- MWC.uniformR @(Network 784 300 100 10) (-0.5, 0.5) g
      return (take n test, net)) $
    \ ~(test0, net0) ->
    bgroup "backprop"
--      [ bgroup "gradient"
--          [ let runTest x y     = gradNetManual x y net0
--            in  bench "handwritten"  $ nf (uncurry runTest) test0
--          , let runTest x y     = gradBP (netErr x y) net0
--            in  bench "bp-lens" $ nf (uncurry runTest) test0
--          , let runTest x y     = gradBP (netErrHKD x y) net0
--            in  bench "bp-hkd"  $ nf (uncurry runTest) test0
--          , let runTest x y     = gradBP (\n' -> netErrHybrid n' y x) net0
--            in  bench "hybrid"  $ nf (uncurry runTest) test0
--          ]
      [ let runTest !net (x, y) = trainStepManual 0.02 x y net
        in  bench "handwritten"  $ nf (trainMnist2 runTest net0) test0
      , let runTest !net (x, y) = trainStep 0.02 x y net
        in  bench "bp-lens" $ nf (trainMnist2 runTest net0) test0
      , let runTest !net (x, y) = trainStepHKD 0.02 x y net
        in  bench "bp-hkd"  $ nf (trainMnist2 runTest net0) test0
--          , let runTest x y     = trainStepHybrid 0.02 x y net0
--            in  bench "hybrid"  $ nf (uncurry runTest) test0
--      , bgroup "test"
--          [ let runTest         = runNetManual net0
--            in  bench "handwritten"  $ nf (testMnist2 runTest) test0
--          , let runTest x       = evalBP (`runNetwork` x) net0
--            in  bench "bp-lens" $ nf (testMnist2 runTest) test0
--          , let runTest x       = evalBP (`runNetworkHKD` x) net0
--            in  bench "bp-hkd"  $ nf (testMnist2 runTest) test0
--          , let runTest x       = evalBP (`runNetHybrid` x) net0
--            in  bench "hybrid"  $ nf runTest (fst test0)
--          ]
      ]

-- I don't know what extra overhead (and dependencies) a proper type-level
-- solution would incur, so let's just copy-paste.
backpropBgroupStorable125_50 :: [( Data.Vector.Storable.Vector Double
                                 , Data.Vector.Storable.Vector Double )]
                             -> Int
                             -> Benchmark
backpropBgroupStorable125_50 test n =
  let f (glyphs, labels) =
        ( fromJust $ create glyphs
        , fromJust $ create labels )
  in backpropBgroup125_50 (map f test) n

backpropBgroup125_50 :: [(R 784, R 10)] -> Int -> Benchmark
backpropBgroup125_50 test n =
    env (do
      g <- MWC.initialize
           . V.fromList
           . map (fromIntegral . ord)
           $ "hello world"
      net <- MWC.uniformR @(Network 784 125 50 10) (-0.5, 0.5) g
      return (take n test, net)) $
    \ ~(test0, net0) ->
    bgroup "backprop"
--      [ bgroup "gradient"
--          [ let runTest x y     = gradNetManual x y net0
--            in  bench "handwritten"  $ nf (uncurry runTest) test0
--          , let runTest x y     = gradBP (netErr x y) net0
--            in  bench "bp-lens" $ nf (uncurry runTest) test0
--          , let runTest x y     = gradBP (netErrHKD x y) net0
--            in  bench "bp-hkd"  $ nf (uncurry runTest) test0
--          , let runTest x y     = gradBP (\n' -> netErrHybrid n' y x) net0
--            in  bench "hybrid"  $ nf (uncurry runTest) test0
--          ]
      [ let runTest !net (x, y) = trainStepManual 0.02 x y net
        in  bench "handwritten"  $ nf (trainMnist2 runTest net0) test0
      , let runTest !net (x, y) = trainStep 0.02 x y net
        in  bench "bp-lens" $ nf (trainMnist2 runTest net0) test0
      , let runTest !net (x, y) = trainStepHKD 0.02 x y net
        in  bench "bp-hkd"  $ nf (trainMnist2 runTest net0) test0
--          , let runTest x y     = trainStepHybrid 0.02 x y net0
--            in  bench "hybrid"  $ nf (uncurry runTest) test0
--      , bgroup "test"
--          [ let runTest         = runNetManual net0
--            in  bench "handwritten"  $ nf (testMnist2 runTest) test0
--          , let runTest x       = evalBP (`runNetwork` x) net0
--            in  bench "bp-lens" $ nf (testMnist2 runTest) test0
--          , let runTest x       = evalBP (`runNetworkHKD` x) net0
--            in  bench "bp-hkd"  $ nf (testMnist2 runTest) test0
--          , let runTest x       = evalBP (`runNetHybrid` x) net0
--            in  bench "hybrid"  $ nf runTest (fst test0)
--          ]
      ]

backpropBgroupStorable1000_300 :: [( Data.Vector.Storable.Vector Double
                                   , Data.Vector.Storable.Vector Double )]
                               -> Int
                               -> Benchmark
backpropBgroupStorable1000_300 test n =
  let f (glyphs, labels) =
        ( fromJust $ create glyphs
        , fromJust $ create labels )
  in backpropBgroup1000_300 (map f test) n

backpropBgroup1000_300 :: [(R 784, R 10)] -> Int -> Benchmark
backpropBgroup1000_300 test n =
    env (do
      g <- MWC.initialize
           . V.fromList
           . map (fromIntegral . ord)
           $ "hello world"
      net <- MWC.uniformR @(Network 784 1000 300 10) (-0.5, 0.5) g
      return (take n test, net)) $
    \ ~(test0, net0) ->
    bgroup "backprop"
--      [ bgroup "gradient"
--          [ let runTest x y     = gradNetManual x y net0
--            in  bench "handwritten"  $ nf (uncurry runTest) test0
--          , let runTest x y     = gradBP (netErr x y) net0
--            in  bench "bp-lens" $ nf (uncurry runTest) test0
--          , let runTest x y     = gradBP (netErrHKD x y) net0
--            in  bench "bp-hkd"  $ nf (uncurry runTest) test0
--          , let runTest x y     = gradBP (\n' -> netErrHybrid n' y x) net0
--            in  bench "hybrid"  $ nf (uncurry runTest) test0
--          ]
      [ let runTest !net (x, y) = trainStepManual 0.02 x y net
        in  bench "handwritten"  $ nf (trainMnist2 runTest net0) test0
      , let runTest !net (x, y) = trainStep 0.02 x y net
        in  bench "bp-lens" $ nf (trainMnist2 runTest net0) test0
      , let runTest !net (x, y) = trainStepHKD 0.02 x y net
        in  bench "bp-hkd"  $ nf (trainMnist2 runTest net0) test0
--          , let runTest x y     = trainStepHybrid 0.02 x y net0
--            in  bench "hybrid"  $ nf (uncurry runTest) test0
--      , bgroup "test"
--          [ let runTest         = runNetManual net0
--            in  bench "handwritten"  $ nf (testMnist2 runTest) test0
--          , let runTest x       = evalBP (`runNetwork` x) net0
--            in  bench "bp-lens" $ nf (testMnist2 runTest) test0
--          , let runTest x       = evalBP (`runNetworkHKD` x) net0
--            in  bench "bp-hkd"  $ nf (testMnist2 runTest) test0
--          , let runTest x       = evalBP (`runNetHybrid` x) net0
--            in  bench "hybrid"  $ nf runTest (fst test0)
--          ]
      ]

testMnist2 :: (R 784 -> R 10) -> [(R 784, R 10)] -> Double
{-# INLINE testMnist2 #-}
testMnist2 f xs =
  let matchesLabels :: (R 784, R 10) -> Bool
      matchesLabels (glyph, label) =
        let value = f glyph
        in HM.maxIndex (extract value) == HM.maxIndex (extract label)
  in fromIntegral (length (filter matchesLabels xs)) / fromIntegral (length xs)

trainMnist2 :: (Network 784 h1 h2 10
                -> (R 784, R 10)
                -> Network 784 h1 h2 10)
            -> Network 784 h1 h2 10
            -> [(R 784, R 10)]
            -> Network 784 h1 h2 10
{-# INLINE trainMnist2 #-}
trainMnist2 = foldl'


plot500150 :: [( Data.Vector.Storable.Vector Double
               , Data.Vector.Storable.Vector Double )]
           -> IO ()
plot500150 test0 = do
  let f (glyphs, labels) =
        ( fromJust $ create glyphs
        , fromJust $ create labels )
      test :: [(R 784, R 10)]
      !test = map f test0
  g <- MWC.initialize
       . V.fromList
       . map (fromIntegral . ord)
       $ "hello world"
  !net0 <- MWC.uniformR @(Network 784 500 150 10) (-0.5, 0.5) g
  let runTest (!net, !times) (x, y) = do
        let (!netNew, !value) = trainStepManual2 0.02 x y net
        time <- getPOSIXTime
        return (netNew, (time, value) : times)
  timeBefore <- getPOSIXTime
  (netOut0, timesOut0) <- foldM runTest (net0, []) test
  let !trainData = shuffle (mkStdGen 7) test
  (_netOut, timesOut) <- foldM runTest (netOut0, timesOut0) trainData
  let ppTime (t, l) = init (show (t - timeBefore)) ++ " " ++ show l
  writeFile "walltimeLoss.txt" $ unlines $ map ppTime timesOut


-- ------------------------------
-- - "Backprop" Lens Mode       -
-- ------------------------------

runLayer
    :: (KnownNat i, KnownNat o, Reifies s W)
    => BVar s (Layer i o)
    -> BVar s (R i)
    -> BVar s (R o)
runLayer l x = (l ^^. lWeights) #>! x + (l ^^. lBiases)
{-# INLINE runLayer #-}

softMax :: (KnownNat n, Reifies s W) => BVar s (R n) -> BVar s (R n)
softMax x = konst' (1 / sumElements' expx) * expx
  where
    expx = exp x
{-# INLINE softMax #-}

runNetwork
    :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o, Reifies s W)
    => BVar s (Network i h1 h2 o)
    -> R i
    -> BVar s (R o)
runNetwork n = softMax
             . runLayer (n ^^. nLayer3)
             . logistic
             . runLayer (n ^^. nLayer2)
             . logistic
             . runLayer (n ^^. nLayer1)
             . auto
{-# INLINE runNetwork #-}

crossEntropy
    :: (KnownNat n, Reifies s W)
    => R n
    -> BVar s (R n)
    -> BVar s Double
crossEntropy t r = negate $ log r <.>! auto t
{-# INLINE crossEntropy #-}

netErr
    :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o, Reifies s W)
    => R i
    -> R o
    -> BVar s (Network i h1 h2 o)
    -> BVar s Double
netErr x t n = crossEntropy t (runNetwork n x)
{-# INLINE netErr #-}

trainStep
    :: forall i h1 h2 o. (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => Double
    -> R i
    -> R o
    -> Network i h1 h2 o
    -> Network i h1 h2 o
trainStep r !x !t !n = n - realToFrac r * gradBP (netErr x t) n
{-# INLINE trainStep #-}

-- ------------------------------
-- - "Backprop" HKD Mode        -
-- ------------------------------

runLayerHKD
    :: (KnownNat i, KnownNat o, Reifies s W)
    => BVar s (Layer i o)
    -> BVar s (R i)
    -> BVar s (R o)
runLayerHKD (splitBV->Layer w b) x = w #>! x + b
{-# INLINE runLayerHKD #-}

runNetworkHKD
    :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o, Reifies s W)
    => BVar s (Network i h1 h2 o)
    -> R i
    -> BVar s (R o)
runNetworkHKD (splitBV->Net l1 l2 l3) = softMax
                                      . runLayerHKD l3
                                      . logistic
                                      . runLayerHKD l2
                                      . logistic
                                      . runLayerHKD l1
                                      . auto
{-# INLINE runNetworkHKD #-}

netErrHKD
    :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o, Reifies s W)
    => R i
    -> R o
    -> BVar s (Network i h1 h2 o)
    -> BVar s Double
netErrHKD x t n = crossEntropy t (runNetworkHKD n x)
{-# INLINE netErrHKD #-}

trainStepHKD
    :: forall i h1 h2 o. (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => Double
    -> R i
    -> R o
    -> Network i h1 h2 o
    -> Network i h1 h2 o
trainStepHKD r !x !t !n = n - realToFrac r * gradBP (netErrHKD x t) n
{-# INLINE trainStepHKD #-}

-- ------------------------------
-- - "Manual" Mode              -
-- ------------------------------

runLayerManual
    :: (KnownNat i, KnownNat o)
    => Layer i o
    -> R i
    -> R o
runLayerManual l x = (l ^. lWeights) #> x + (l ^. lBiases)
{-# INLINE runLayerManual #-}

softMaxManual :: KnownNat n => R n -> R n
softMaxManual x = konst (1 / sumElements expx) * expx
  where
    expx = exp x
{-# INLINE softMaxManual #-}

runNetManual
    :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => Network i h1 h2 o
    -> R i
    -> R o
runNetManual n = softMaxManual
               . runLayerManual (n ^. nLayer3)
               . logistic
               . runLayerManual (n ^. nLayer2)
               . logistic
               . runLayerManual (n ^. nLayer1)
{-# INLINE runNetManual #-}

gradNetManual
    :: forall i h1 h2 o. (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => R i
    -> R o
    -> Network i h1 h2 o
    -> Network i h1 h2 o
gradNetManual x t (Net (Layer w1 b1) (Layer w2 b2) (Layer w3 b3)) =
    let y1 = w1 #> x
        z1 = y1 + b1
        x2 = logistic z1
        y2 = w2 #> x2
        z2 = y2 + b2
        x3 = logistic z2
        y3 = w3 #> x3
        z3 = y3 + b3
        o0 = exp z3
        o1 = HM.sumElements (extract o0)
        o2 = o0 / konst o1
        -- o3 = - (log o2 <.> t)
        dEdO3 = 1
        dEdO2 = dEdO3 * (- t / o2)
        dEdO1 = - (dEdO2 <.> o0) / (o1 ** 2)
        dEdO0 = konst dEdO1 + dEdO2 / konst o1
        dEdZ3 = dEdO0 * o0
        dEdY3 = dEdZ3
        dEdX3 = tr w3 #> dEdY3
        dEdZ2 = dEdX3 * (x3 * (1 - x3))
        dEdY2 = dEdZ2
        dEdX2 = tr w2 #> dEdY2
        dEdZ1 = dEdX2 * (x2 * (1 - x2))
        dEdY1 = dEdZ1
        dEdB3 = dEdZ3
        dEdW3 = dEdY3 `outer` x3
        dEdB2 = dEdZ2
        dEdW2 = dEdY2 `outer` x2
        dEdB1 = dEdZ1
        dEdW1 = dEdY1 `outer` x
    in  Net (Layer dEdW1 dEdB1) (Layer dEdW2 dEdB2) (Layer dEdW3 dEdB3)
{-# INLINE gradNetManual #-}

gradNetManual2
    :: forall i h1 h2 o. (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => R i
    -> R o
    -> Network i h1 h2 o
    -> (Network i h1 h2 o, Double)
gradNetManual2 x t (Net (Layer w1 b1) (Layer w2 b2) (Layer w3 b3)) =
    let y1 = w1 #> x
        z1 = y1 + b1
        x2 = logistic z1
        y2 = w2 #> x2
        z2 = y2 + b2
        x3 = logistic z2
        y3 = w3 #> x3
        z3 = y3 + b3
        o0 = exp z3
        o1 = HM.sumElements (extract o0)
        o2 = o0 / konst o1
        !o3 = - (log o2 <.> t)
        dEdO3 = 1
        dEdO2 = dEdO3 * (- t / o2)
        dEdO1 = - (dEdO2 <.> o0) / (o1 ** 2)
        dEdO0 = konst dEdO1 + dEdO2 / konst o1
        dEdZ3 = dEdO0 * o0
        dEdY3 = dEdZ3
        dEdX3 = tr w3 #> dEdY3
        dEdZ2 = dEdX3 * (x3 * (1 - x3))
        dEdY2 = dEdZ2
        dEdX2 = tr w2 #> dEdY2
        dEdZ1 = dEdX2 * (x2 * (1 - x2))
        dEdY1 = dEdZ1
        dEdB3 = dEdZ3
        dEdW3 = dEdY3 `outer` x3
        dEdB2 = dEdZ2
        dEdW2 = dEdY2 `outer` x2
        dEdB1 = dEdZ1
        dEdW1 = dEdY1 `outer` x
    in  (Net (Layer dEdW1 dEdB1) (Layer dEdW2 dEdB2) (Layer dEdW3 dEdB3), o3)
{-# INLINE gradNetManual2 #-}

trainStepManual
    :: forall i h1 h2 o. (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => Double
    -> R i
    -> R o
    -> Network i h1 h2 o
    -> Network i h1 h2 o
trainStepManual r !x !t !n =
    let gN = gradNetManual x t n
    in  n - (realToFrac r * gN)

trainStepManual2
    :: forall i h1 h2 o. (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => Double
    -> R i
    -> R o
    -> Network i h1 h2 o
    -> (Network i h1 h2 o, Double)
trainStepManual2 r !x !t !n =
    let (gN, value) = gradNetManual2 x t n
    in  (n - (realToFrac r * gN), value)

-- ------------------------------
-- - "Hybrid" Mode              -
-- ------------------------------

layerOp :: (KnownNat i, KnownNat o) => Op '[Layer i o, R i] (R o)
layerOp = op2 $ \(Layer w b) x ->
    ( w #> x + b
    , \g -> (Layer (g `outer` x) g, tr w #> g)
    )
{-# INLINE layerOp #-}

logisticOp
    :: Floating a
    => Op '[a] a
logisticOp = op1 $ \x ->
    let lx = logistic x
    in  (lx, \g -> lx * (1 - lx) * g)
{-# INLINE logisticOp #-}

softMaxOp
    :: KnownNat n
    => Op '[R n] (R n)
softMaxOp = op1 $ \x ->
    let expx   = exp x
        tot    = sumElements expx
        invtot = 1 / tot
        res    = konst invtot * expx
    in  ( res
        , \g -> res - konst (invtot ** 2) * exp (2 * x) * g
        )
{-# INLINE softMaxOp #-}

softMaxCrossEntropyOp
    :: KnownNat n
    => R n
    -> Op '[R n] Double
softMaxCrossEntropyOp targ = op1 $ \x ->
    let expx   = exp x
        sm     = konst (1 / sumElements expx) * expx
        ce     = negate $ log sm <.> targ
    in  ( ce
        , \g -> (sm - targ) * konst g
        )
{-# INLINE softMaxCrossEntropyOp #-}

_runNetHybrid
    :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o, Reifies s W)
    => BVar s (Network i h1 h2 o)
    -> R i
    -> BVar s (R o)
_runNetHybrid n = liftOp1 softMaxOp
               . liftOp2 layerOp (n ^^. nLayer3)
               . liftOp1 logisticOp
               . liftOp2 layerOp (n ^^. nLayer2)
               . liftOp1 logisticOp
               . liftOp2 layerOp (n ^^. nLayer1)
               . auto
{-# INLINE _runNetHybrid #-}

_netErrHybrid
    :: (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o, Reifies s W)
    => BVar s (Network i h1 h2 o)
    -> R o
    -> R i
    -> BVar s Double
_netErrHybrid n t = liftOp1 (softMaxCrossEntropyOp t)
                 . liftOp2 layerOp (n ^^. nLayer3)
                 . liftOp1 logisticOp
                 . liftOp2 layerOp (n ^^. nLayer2)
                 . liftOp1 logisticOp
                 . liftOp2 layerOp (n ^^. nLayer1)
                 . auto
{-# INLINE _netErrHybrid #-}

_trainStepHybrid
    :: forall i h1 h2 o. (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o)
    => Double
    -> R i
    -> R o
    -> Network i h1 h2 o
    -> Network i h1 h2 o
_trainStepHybrid r !x !t !n =
    let gN = gradBP (\n' -> _netErrHybrid n' t x) n
    in  n - (realToFrac r * gN)
{-# INLINE _trainStepHybrid #-}

-- ------------------------------
-- - Operations                 -
-- ------------------------------

infixr 8 #>!
(#>!)
    :: (KnownNat m, KnownNat n, Reifies s W)
    => BVar s (L m n)
    -> BVar s (R n)
    -> BVar s (R m)
(#>!) = liftOp2 . op2 $ \m v ->
  ( m #> v, \g -> (g `outer` v, tr m #> g) )
{-# INLINE (#>!) #-}

infixr 8 <.>!
(<.>!)
    :: (KnownNat n, Reifies s W)
    => BVar s (R n)
    -> BVar s (R n)
    -> BVar s Double
(<.>!) = liftOp2 . op2 $ \x y ->
  ( x <.> y, \g -> (konst g * y, x * konst g)
  )
{-# INLINE (<.>!) #-}

konst'
    :: (KnownNat n, Reifies s W)
    => BVar s Double
    -> BVar s (R n)
konst' = liftOp1 . op1 $ \c -> (konst c, HM.sumElements . extract)
{-# INLINE konst' #-}

sumElements :: KnownNat n => R n -> Double
sumElements = HM.sumElements . extract
{-# INLINE sumElements #-}

sumElements'
    :: (KnownNat n, Reifies s W)
    => BVar s (R n)
    -> BVar s Double
sumElements' = liftOp1 . op1 $ \x -> (sumElements x, konst)
{-# INLINE sumElements' #-}

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))
{-# INLINE logistic #-}

-- ------------------------------
-- - Instances                  -
-- ------------------------------

instance (KnownNat i, KnownNat o) => Num (Layer i o) where
    Layer w1 b1 + Layer w2 b2 = Layer (w1 + w2) (b1 + b2)
    Layer w1 b1 - Layer w2 b2 = Layer (w1 - w2) (b1 - b2)
    Layer w1 b1 * Layer w2 b2 = Layer (w1 * w2) (b1 * b2)
    abs    (Layer w b)        = Layer (abs    w) (abs    b)
    signum (Layer w b)        = Layer (signum w) (signum b)
    negate (Layer w b)        = Layer (negate w) (negate b)
    fromInteger x             = Layer (fromInteger x) (fromInteger x)

instance (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o) => Num (Network i h1 h2 o) where
    Net a b c + Net d e f = Net (a + d) (b + e) (c + f)
    Net a b c - Net d e f = Net (a - d) (b - e) (c - f)
    Net a b c * Net d e f = Net (a * d) (b * e) (c * f)
    abs    (Net a b c)    = Net (abs    a) (abs    b) (abs    c)
    signum (Net a b c)    = Net (signum a) (signum b) (signum c)
    negate (Net a b c)    = Net (negate a) (negate b) (negate c)
    fromInteger x         = Net (fromInteger x) (fromInteger x) (fromInteger x)

instance (KnownNat i, KnownNat o) => Fractional (Layer i o) where
    Layer w1 b1 / Layer w2 b2 = Layer (w1 / w2) (b1 / b2)
    recip (Layer w b)         = Layer (recip w) (recip b)
    fromRational x            = Layer (fromRational x) (fromRational x)

instance (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o) => Fractional (Network i h1 h2 o) where
    Net a b c / Net d e f = Net (a / d) (b / e) (c / f)
    recip (Net a b c)     = Net (recip a) (recip b) (recip c)
    fromRational x        = Net (fromRational x) (fromRational x) (fromRational x)

instance KnownNat n => MWC.Variate (R n) where
    uniform g = randomVector <$> MWC.uniform g <*> pure Uniform
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (KnownNat m, KnownNat n) => MWC.Variate (L m n) where
    uniform g = uniformSample <$> MWC.uniform g <*> pure 0 <*> pure 1
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (KnownNat i, KnownNat o) => MWC.Variate (Layer i o) where
    uniform g = Layer <$> MWC.uniform g <*> MWC.uniform g
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o) => MWC.Variate (Network i h1 h2 o) where
    uniform g = Net <$> MWC.uniform g <*> MWC.uniform g <*> MWC.uniform g
    uniformR (l, h) g = (\x -> x * (h - l) + l) <$> MWC.uniform g

instance Backprop (R n) where
    zero = zeroNum
    add  = addNum
    one  = oneNum

instance (KnownNat n, KnownNat m) => Backprop (L m n) where
    zero = zeroNum
    add  = addNum
    one  = oneNum

instance (KnownNat i, KnownNat o) => Backprop (Layer i o)
instance (KnownNat i, KnownNat h1, KnownNat h2, KnownNat o) => Backprop (Network i h1 h2 o)
