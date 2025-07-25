module Main (main) where

import           Prelude

import           Control.Arrow       ((***))
import           Criterion.Main
import qualified Data.Vector.Generic as V
import           System.Random

import qualified BenchMnistTools
import qualified MnistAdTools
import qualified MnistBackpropTools
import           MnistData

main :: IO ()
main = do
  testData0 <- loadMnistData testGlyphsPath testLabelsPath  -- 10k total
  let testData = shuffle (mkStdGen 42) testData0
  defaultMain
    [ env (return $ take 100 testData) $
      \ xs ->
      bgroup "125|50"
        [ env (return $! map mkMnistDataLinearR xs) $
          \ vs ->
          bgroup "horde-ad"
            [ BenchMnistTools.mnistTrainBench1VTA "" 125 50 0.02 100 vs
            , BenchMnistTools.mnistTrainBench1VTO "" 125 50 0.02 100 vs
            , BenchMnistTools.mnistTrainBench2VTA "" 125 50 0.02 100 vs
            , BenchMnistTools.mnistTrainBench2VTO "" 125 50 0.02 100 vs
            ]
        , MnistBackpropTools.backpropBgroupStorable12550 xs 100
        , env (return $! map (V.convert *** V.convert) xs) $
          \ vs ->
          bgroup "ad"
            [ MnistAdTools.mnistTrainBench2 100 vs 125 50 0.02
            ]
        ]
    , env (return $ take 100 testData) $
      \ xs ->
      bgroup "500|150"
        [ env (return $! map mkMnistDataLinearR xs) $
          \ vs ->
          bgroup "horde-ad"
            [ BenchMnistTools.mnistTrainBench1VTA "" 500 150 0.02 100 vs
            , BenchMnistTools.mnistTrainBench1VTO "" 500 150 0.02 100 vs
            , BenchMnistTools.mnistTrainBench2VTA "" 500 150 0.02 100 vs
            , BenchMnistTools.mnistTrainBench2VTO "" 500 150 0.02 100 vs
            ]
        , MnistBackpropTools.backpropBgroupStorable500150 xs 100
-- too slow
--        , env (return $! map (V.convert *** V.convert) xs) $
--          \ vs ->
--          bgroup "ad"
--            [ MnistAdTools.mnistTrainBench2 100 vs 500 150 0.02
--            , MnistAdTools.mnistTestBench2 100 vs 500 150
--            ]
        ]
    , env (return $ take 100 testData) $
      \ xs ->
      bgroup "2911|811"
        [ env (return $! map mkMnistDataLinearR xs) $
          \ vs ->
          bgroup "horde-ad"
            [ BenchMnistTools.mnistTrainBench1VTA "" 2911 811 0.02 100 vs
            , BenchMnistTools.mnistTrainBench1VTO "" 2911 811 0.02 100 vs
            , BenchMnistTools.mnistTrainBench2VTA "" 2911 811 0.02 100 vs
            , BenchMnistTools.mnistTrainBench2VTO "" 2911 811 0.02 100 vs
            ]
        , MnistBackpropTools.backpropBgroupStorable2911811 xs 100
        ]
    ]
