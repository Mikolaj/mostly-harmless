module Main (main) where

import Prelude

import Control.DeepSeq
import Criterion.Main
import System.Random

import qualified BenchProdTools
import qualified ProdAdTools
import qualified ProdBackpropTools
import qualified ProdManualTools

allxs :: [Double]
allxs = let xs = map (+ 0.55) $ randoms (mkStdGen 42)
        in deepseq (take 50000000 xs) xs

main :: IO ()
main = defaultMain
  [ bgroup "1000"
      [ bgroup "manual"
          [ ProdManualTools.bgroup1000 allxs
          ]
      , bgroup "ours"
          [ BenchProdTools.bgroup1000 allxs
          ]
      , bgroup "ad"
          [ ProdAdTools.bgroup1000 allxs
          ]
      , bgroup "backprop"
          [ ProdBackpropTools.bgroup1000 allxs
          ]
      ]
  , bgroup "1e6"
      [ bgroup "manual"
          [ ProdManualTools.bgroup1e6 allxs
          ]
      , bgroup "ours"
          [ BenchProdTools.bgroup1e6 allxs
          ]
      , bgroup "ad"
          [ ProdAdTools.bgroup1e6 allxs
          ]
      , bgroup "backprop"
          [ ProdBackpropTools.bgroup1e6 allxs
          ]
      ]
  , bgroup "5e7"
      [ bgroup "manual"
          [ ProdManualTools.bgroup5e7 allxs
          ]
      , bgroup "ours"
          [ BenchProdTools.bgroup5e7 allxs
          ]
      , bgroup "ad"
          [ ProdAdTools.bgroup5e7 allxs
          ]
      , bgroup "backprop"
          [ ProdBackpropTools.bgroup5e7 allxs
          ]
      ]
  ]
