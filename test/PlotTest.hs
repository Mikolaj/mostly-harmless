module Main (main) where

import Prelude

import Control.DeepSeq
import System.Random
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)

import           HordeAd.Tool.MnistTools
import qualified MnistBackpropTools
import qualified TestMnistFC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Plot Tests"
  [ testCase "hmatrix/manual 500 150 to file" $ do
      trainData0 <- loadMnistData trainGlyphsPath trainLabelsPath
      let !trainData = force $ shuffle (mkStdGen 6) trainData0
      MnistBackpropTools.plot500150 trainData
  , TestMnistFC.mnistTestCase2T
      True
      "2 epochs, all batches, TL, wider, to file"
      2 60000 nnMnistLoss2L 500 150 0.02
      4.290000000000005e-2
  ]
