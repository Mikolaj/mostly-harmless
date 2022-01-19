{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             RankNTypes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ArrayMnistTools where

import Prelude

import           Codec.Compression.GZip (decompress)
import           Control.Exception (assert)
import qualified Data.ByteString.Lazy as LBS
import           Data.IDX
import           Data.List (sortOn)
import           Data.Maybe (fromMaybe)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable
import qualified Data.Vector.Unboxed
import           Foreign.Storable (Storable)
import           GHC.Exts (inline)
import           Numeric.LinearAlgebra.Array (Array, None (None))
import           Numeric.LinearAlgebra.Array.Util (Coord, coords, fromVector)
import           System.IO (IOMode (ReadMode), withBinaryFile)
import           System.Random

import ArrayAD

-- * General tools

sizeMnistGlyph :: Int
sizeMnistGlyph = 784

sizeMnistLabel :: Int
sizeMnistLabel = 10

-- Actually, a better representation, supported by @Data.IDX@,
-- is an integer label and a picture (the same vector as below).
-- Then we'd use @lossCrossEntropy@ that picks a component according
-- to the label instead of performing a dot product with scaling.
-- This results in much smaller Delta expressions.
-- Our library makes this easy to express and gradients compute fine.
-- OTOH, methods with only matrix operations and graphs can't handle that.
-- However, the goal of the exercise it to implement the same
-- neural net that backprop uses for benchmarks and compare.
type MnistData r = (Array r, Array r)

-- We assume all activation functions finish with a Delta-binding
-- and so we don't have to add one explicitly.
hiddenLayerMnist :: forall m r. Coord r
                 => (DualDelta (Array r) -> m (DualDelta (Array r)))
                 -> Array r
                 -> VecDualDelta (Array r)
                 -> m (DualDelta (Array r))
hiddenLayerMnist factivation x vec = do
  let weights = var vec 0
      biases = var vec 1
  factivation $ weights #>!! x + biases

middleLayerMnist :: forall m r. Coord r
                 => (DualDelta (Array r) -> m (DualDelta (Array r)))
                 -> DualDelta (Array r)
                 -> Int
                 -> VecDualDelta (Array r)
                 -> m (DualDelta (Array r))
middleLayerMnist factivation hiddenVec offset vec = do
  let weights = var vec offset
      biases = var vec (offset + 1)
  factivation $ weights #>! hiddenVec + biases

-- * 2 hidden layers

-- With arrays, this is only informative. Indexing is now small integers.
lenMnist2 :: Int -> Int -> Int
lenMnist2 widthHidden widthHidden2 =
  widthHidden * (sizeMnistGlyph + 1)
  + widthHidden2 * (widthHidden + 1)
  + sizeMnistLabel * (widthHidden2 + 1)

-- Two hidden layers of width @widthHidden@ and (the middle one) @widthHidden2@.
-- Both hidden layers use the same activation function.
nnMnist2 :: (DeltaMonad (Array r) m, Coord r)
         => (DualDelta (Array r) -> m (DualDelta (Array r)))
         -> (DualDelta (Array r) -> m (DualDelta (Array r)))
         -> Array r
         -> VecDualDelta (Array r)
         -> m (DualDelta (Array r))
nnMnist2 factivationHidden factivationOutput x vec = do
  hiddenVec <- inline hiddenLayerMnist factivationHidden x vec
  let offsetMiddle = 2  -- first weights and first biases used up
  middleVec <- inline middleLayerMnist factivationHidden hiddenVec
                                       offsetMiddle vec
  let offsetOutput = 4  -- second weights and second biases used up
  inline middleLayerMnist factivationOutput middleVec
                          offsetOutput vec

nnMnistLoss2 :: ( DeltaMonad (Array r) m, Floating r, Coord r
                , Floating (Data.Vector.Storable.Vector r) )
             => MnistData r
             -> VecDualDelta (Array r)
             -> m (DualDelta (Array r))
nnMnistLoss2 (x, targ) vec = do
  let !_A1 = assert (sizeMnistGlyph == V.length (coords x)) ()
      !_A2 = assert (sizeMnistLabel == V.length (coords targ)) ()
  res <- inline nnMnist2 logisticAct softMaxAct x vec
  lossCrossEntropy targ res

generalTestMnist :: forall r. (Ord r, Fractional r, Storable r)
                 => (Array r
                     -> VecDualDelta (Array r)
                     -> DeltaMonadValue (Array r) (DualDelta (Array r)))
                 -> [MnistData r] -> Domain (Array r)
                 -> r
{-# INLINE generalTestMnist #-}
generalTestMnist nn xs res =
  let matchesLabels :: MnistData r -> Bool
      matchesLabels (glyph, label) =
        let v = coords $ valueDualDelta (nn glyph) res
        in V.maxIndex v == V.maxIndex (coords label)
  in fromIntegral (length (filter matchesLabels xs)) / fromIntegral (length xs)

testMnist2 :: ( Ord r, Floating r, Coord r
              , Floating (Data.Vector.Storable.Vector r) )
           => [MnistData r] -> Domain (Array r) -> r
testMnist2 xs res =
  generalTestMnist (inline nnMnist2 logisticAct softMaxAct) xs res

-- * Reading data files

readMnistData :: LBS.ByteString -> LBS.ByteString -> [MnistData Double]
readMnistData glyphsBS labelsBS =
  let glyphs = fromMaybe (error "wrong MNIST glyphs file")
               $ decodeIDX glyphsBS
      labels = fromMaybe (error "wrong MNIST labels file")
               $ decodeIDXLabels labelsBS
      intData = fromMaybe (error "can't decode MNIST file into integers")
                $ labeledIntData labels glyphs
      f :: (Int, Data.Vector.Unboxed.Vector Int) -> MnistData Double
      -- Copied from library backprop to enable comparison of results.
      -- I have no idea how this is different from @labeledDoubleData@, etc.
      f (labN, v) =
        let labD :: Data.Vector.Unboxed.Vector Double
            labD = V.generate sizeMnistLabel (\i -> if i == labN then 1 else 0)
        in ( fromVector None $ V.convert $ V.map (\r -> fromIntegral r / 255) v
           , fromVector None $ V.convert labD )
  in map f intData

trainGlyphsPath, trainLabelsPath, testGlyphsPath, testLabelsPath :: FilePath
trainGlyphsPath = "samplesData/train-images-idx3-ubyte.gz"
trainLabelsPath = "samplesData/train-labels-idx1-ubyte.gz"
testGlyphsPath  = "samplesData/t10k-images-idx3-ubyte.gz"
testLabelsPath  = "samplesData/t10k-labels-idx1-ubyte.gz"

loadMnistData :: FilePath -> FilePath -> IO [MnistData Double]
loadMnistData glyphsPath labelsPath =
  withBinaryFile glyphsPath ReadMode $ \glyphsHandle ->
    withBinaryFile labelsPath ReadMode $ \labelsHandle -> do
      glyphsContents <- LBS.hGetContents glyphsHandle
      labelsContents <- LBS.hGetContents labelsHandle
      return $! readMnistData (decompress glyphsContents)
                              (decompress labelsContents)

-- Good enough for QuickCheck, so good enough for me.
shuffle :: RandomGen g => g -> [a] -> [a]
shuffle g l =
  let rnds = randoms g :: [Int]
  in map fst $ sortOn snd $ zip l rnds
