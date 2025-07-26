module Main (main) where

import Prelude

import Control.DeepSeq
import Criterion.Main
import System.Random

import BenchProdTools qualified
import ProdAdTools qualified
import ProdBackpropTools qualified
import ProdManualTools qualified

allxs :: [Double]
allxs = let xs = map (+ 0.55) $ randoms (mkStdGen 42)
        in deepseq (take 50000000 xs) xs

main :: IO ()
main = defaultMain
  [ bgroup "manual gradient"
    [ ProdManualTools.bgroup1000 allxs
    ]
  , bgroup "horde-ad"
    [ BenchProdTools.bgroup1000 allxs
    ]
  , bgroup "ad"
    [ ProdAdTools.bgroup1000 allxs
    ]
  , bgroup "backprop"
    [ ProdBackpropTools.bgroup1000 allxs
    ]
  , bgroup "manual gradient"
    [ ProdManualTools.bgroup1e5 allxs
    ]
  , bgroup "horde-ad"
    [ BenchProdTools.bgroup1e5 allxs
    ]
  , bgroup "ad"
    [ ProdAdTools.bgroup1e5 allxs
    ]
  , bgroup "backprop"
    [ ProdBackpropTools.bgroup1e5 allxs
    ]
  ]
{- too slow:
  , bgroup "1e6"
      [ bgroup "manual"
          [ ProdManualTools.bgroup1e6 allxs
          ]
      , bgroup "horde-ad"
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
      , bgroup "horde-ad"
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
-}
