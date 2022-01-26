module Main (main) where

import Prelude

import           Control.Arrow ((***))
import           Criterion.Main
import qualified Data.Vector.Generic as V
import           HordeAd.MnistTools
import           System.Random

import qualified BenchMnistTools
import qualified MnistAdTools
import qualified MnistBackpropTools

main :: IO ()
main = do
  testData0 <- loadMnistData testGlyphsPath testLabelsPath  -- 10k total
  let testData = shuffle (mkStdGen 42) testData0
  defaultMain
    [ env (return $ take 500 testData) $
      \ xs ->
      bgroup "30 10"
        [ bgroup "ours"
            [ BenchMnistTools.mnistTrainBench2 "" 500 xs 30 10 0.02
            , BenchMnistTools.mnistTestBench2 "" 500 xs 30 10
            ]
        , env (return $ map (V.convert *** V.convert) xs) $
          \ vs ->
          bgroup "ad"
            [ MnistAdTools.mnistTrainBench2 500 vs 30 10 0.02
            , MnistAdTools.mnistTestBench2 500 vs 30 10
            ]
        , MnistBackpropTools.backpropBgroupUnboxed3010 xs 500
        ]
    , env (return $ take 500 testData) $
      \ xs ->
      bgroup "300 100"
        [ bgroup "ours"
            [ BenchMnistTools.mnistTrainBench2 "" 500 xs 300 100 0.02
            , BenchMnistTools.mnistTestBench2 "" 500 xs 300 100
            ]
        , env (return $ map (V.convert *** V.convert) xs) $
          \ vs ->
          bgroup "ad"
            [ MnistAdTools.mnistTrainBench2 500 vs 300 100 0.02
            , MnistAdTools.mnistTestBench2 500 vs 300 100
            ]
        , MnistBackpropTools.backpropBgroupUnboxed xs 500
        ]
    , env (return $ take 500 testData) $
      \ xs ->
      bgroup "500 150"
        [ bgroup "ours"
            [ BenchMnistTools.mnistTrainBench2 "" 500 xs 500 150  0.02
            , BenchMnistTools.mnistTestBench2 "" 500 xs 500 150
            ]
-- too slow
--        , env (return $ map (V.convert *** V.convert) xs) $
--          \ vs ->
--          bgroup "ad"
--            [ MnistAdTools.mnistTrainBench2 500 vs 500 150 0.02
--            , MnistAdTools.mnistTestBench2 500 vs 500 150
--            ]
        , MnistBackpropTools.backpropBgroupUnboxed500150 xs 500
        ]
    ]
