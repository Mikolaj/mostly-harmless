module Main (main) where

import Prelude

import Control.Arrow ((***))
import Criterion.Main
import Data.Vector.Generic qualified as V
import System.Random

import BenchMnistTools qualified
import MnistAdTools qualified
import MnistBackpropTools qualified
import MnistData

main :: IO ()
main = do
  testData0 <- loadMnistData testGlyphsPath testLabelsPath  -- 10k total
  let testData = shuffle (mkStdGen 42) testData0
  defaultMain
    [ env (return $ take 100 testData) $
      \ xs ->
      bgroup "widths 125 50"
        [ -- The horde-ad version additionally keeps some parameters
          -- as Float and converts all the time between Float and Double,
          -- but this is not a significant overhead.
          env (return $! map mkMnistDataLinearR xs) $
          \ vs ->
          bgroup "horde-ad"
            [ BenchMnistTools.mnistTrainBench1VTA "" 125 50 0.02 100 vs
            , BenchMnistTools.mnistTrainBench1VTO "" 125 50 0.02 100 vs
            , BenchMnistTools.mnistTrainBench2VTA "" 125 50 0.02 100 vs
            , BenchMnistTools.mnistTrainBench2VTO "" 125 50 0.02 100 vs
            ]
        , MnistBackpropTools.backpropBgroupStorable125_50 xs 100
        , env (return $! map (V.convert *** V.convert) xs) $
          \ vs ->
          bgroup "ad"
            [ MnistAdTools.mnistTrainBench2 100 vs 125 50 0.02
            ]
        ]
    , env (return $ take 100 testData) $
      \ xs ->
      bgroup "widths 1000 300"
        [ env (return $! map mkMnistDataLinearR xs) $
          \ vs ->
          bgroup "horde-ad"
            [ BenchMnistTools.mnistTrainBench1VTA "" 1000 300 0.02 100 vs
            , BenchMnistTools.mnistTrainBench1VTO "" 1000 300 0.02 100 vs
            , BenchMnistTools.mnistTrainBench2VTA "" 1000 300 0.02 100 vs
            , BenchMnistTools.mnistTrainBench2VTO "" 1000 300 0.02 100 vs
            ]
        , MnistBackpropTools.backpropBgroupStorable1000_300 xs 100
        ]
    ]
