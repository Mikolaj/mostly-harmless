module Main (main) where

import Prelude

import           Control.Arrow ((***))
import           Criterion.Main
import qualified Data.Vector.Generic as V
import           HordeAd.Tool.MnistTools
import           System.Random

import qualified BenchMnistTools
import qualified MnistAdTools
import qualified MnistBackpropTools

main :: IO ()
main = do
  testData0 <- loadMnistData testGlyphsPath testLabelsPath  -- 10k total
  let testData = shuffle (mkStdGen 42) testData0
  defaultMain
    [ env (return $ take 100 testData) $
      \ xs ->
      bgroup "125|50"
        [ bgroup "horde-ad"
            [ BenchMnistTools.mnistTrainBench2 "" 100 xs 125 50 0.02
            , BenchMnistTools.mnistTrainBench2V "" 100 xs 125 50 0.02
            , BenchMnistTools.mnistTrainBench2L "" 100 xs 125 50 0.02
--            , BenchMnistTools.mnistTestBench2 "" 100 xs 125 50
            ]
        , MnistBackpropTools.backpropBgroupStorable12550 xs 100
        , env (return $ map (V.convert *** V.convert) xs) $
          \ vs ->
          bgroup "ad"
            [ MnistAdTools.mnistTrainBench2 100 vs 125 50 0.02
--            , MnistAdTools.mnistTestBench2 100 vs 125 50
            ]
        ]
    , env (return $ take 100 testData) $
      \ xs ->
      bgroup "500|150"
        [ bgroup "horde-ad"
            [ BenchMnistTools.mnistTrainBench2 "" 100 xs 500 150 0.02
            , BenchMnistTools.mnistTrainBench2V "" 100 xs 500 150 0.02
            , BenchMnistTools.mnistTrainBench2L "" 100 xs 500 150 0.02
--            , BenchMnistTools.mnistTestBench2 "" 100 xs 500 150
            ]
        , MnistBackpropTools.backpropBgroupStorable500150 xs 100
-- too slow
--        , env (return $ map (V.convert *** V.convert) xs) $
--          \ vs ->
--          bgroup "ad"
--            [ MnistAdTools.mnistTrainBench2 100 vs 500 150 0.02
--            , MnistAdTools.mnistTestBench2 100 vs 500 150
--            ]
        ]
    , env (return $ take 100 testData) $
      \ xs ->
      bgroup "2911|811"
        [ bgroup "horde-ad"
--            [ BenchMnistTools.mnistTrainBench2 "" 100 xs 2911 811 0.02
            [ BenchMnistTools.mnistTrainBench2V "" 100 xs 2911 811 0.02
            , BenchMnistTools.mnistTrainBench2L "" 100 xs 2911 811 0.02
--            , BenchMnistTools.mnistTestBench2 "" 100 xs 2911 811
            ]
        , MnistBackpropTools.backpropBgroupStorable2911811 xs 100
        ]
    ]
