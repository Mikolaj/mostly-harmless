module Main (main) where

import Prelude

import           Criterion.Main
import qualified Data.Vector
import qualified Data.Vector.Generic as V
import           System.Random

import Numeric.AD.Mode.Reverse.Double hiding (diff)

main :: IO ()
main = do
  let allxs = map (+ 0.55) $ randoms (mkStdGen 42) :: [Double]
      vec100 = V.fromList $ take 100 allxs
      vec200 = V.fromList $ take 200 allxs
      vec1000 = V.fromList $ take 1000 allxs
      vec1000000 = V.fromList $ take 1000000 allxs
      vec10000000 = V.fromList $ take 10000000 allxs
  defaultMain
    [ bgroup "100"
        [ bench "func" $ nf prod (take 100 allxs)
        , bench "grad" $ nf grad_prod (take 100 allxs)
        , bench "vec_func" $ nf vec_prod vec100
        , bench "vec_grad" $ nf vec_grad_prod vec100
        ]
    , bgroup "200"
        [ bench "func" $ nf prod (take 200 allxs)
        , bench "grad" $ nf grad_prod (take 200 allxs)
        , bench "vec_func" $ nf vec_prod vec200
        , bench "vec_grad" $ nf vec_grad_prod vec200
        ]
    , bgroup "1000"
        [ bench "func" $ nf prod (take 1000 allxs)
        , bench "grad" $ nf grad_prod (take 1000 allxs)
        , bench "vec_func" $ nf vec_prod vec1000
        , bench "vec_grad" $ nf vec_grad_prod vec1000
        ]
    , bgroup "1000000"
        [ bench "func" $ nf prod (take 1000000 allxs)
        , bench "grad" $ nf grad_prod (take 1000000 allxs)
        , bench "vec_func" $ nf vec_prod vec1000000
        , bench "vec_grad" $ nf vec_grad_prod vec1000000
        ]
    , bgroup "10000000"
        [ bench "func" $ nf prod (take 10000000 allxs)
        , bench "grad" $ nf grad_prod (take 10000000 allxs)
        , bench "vec_func" $ nf vec_prod vec10000000
        , bench "vec_grad" $ nf vec_grad_prod vec10000000
        ]
    ]

prod :: Num r => [r] -> r
prod [x] = x
prod (x : xs) = x * prod xs

grad_prod :: [Double] -> [Double]
grad_prod = grad prod

-- This benchmark shows that there is slowdown from using vectors
-- with the ad library. The Traversable data structure is probably
-- converted to a list ASAP. Conversion of lists to lists is free,
-- so that's probably the best structure to use.
vec_prod :: forall r. Num r
         => Data.Vector.Vector r -> r
vec_prod = V.foldl1' (*)

vec_grad_prod :: Data.Vector.Vector Double -> Data.Vector.Vector Double
vec_grad_prod = grad vec_prod