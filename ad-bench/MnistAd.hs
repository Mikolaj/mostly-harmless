module Main (main) where

import Prelude

import Criterion.Main
import HordeAd.Tool.MnistTools
import System.Random

import MnistAdTools

main :: IO ()
main = do
  testData0 <- loadMnistData testGlyphsPath testLabelsPath  -- 10k total
  let testData = shuffle (mkStdGen 42) testData0
  defaultMain
    [ mnistTrainBGroup2 testData 100
--    , mnistTrainBGroup2 testData 5000  -- ordinary chunk size, takes too long
    ]
