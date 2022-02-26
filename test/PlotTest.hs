module Main (main) where

import Prelude

import Test.Tasty

import qualified BenchMnistTools
import           HordeAd.Tool.MnistTools
import qualified TestMnistFC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Plot Tests"
  [ testCase "hmatrix/manual 500 150 to file" do
      trainData <- loadMnistData trainGlyphsPath trainLabelsPath
      let testData = shuffle (mkStdGen 6) testData0
      plot500150 testData
  , mnistTestCase2T True
                    "1 epoch, all batches, TL, wider, to file"
                    1 60000 nnMnistLoss2L 500 150 0.02
                    3.73e-2
  ]
