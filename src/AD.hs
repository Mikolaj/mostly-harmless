{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module AD where

import Prelude

import           Control.Exception (assert)
import           Control.Monad (foldM, when)
import           Control.Monad.ST.Strict (ST)
import           Control.Monad.Trans.State.Strict
import qualified Data.Vector
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Unboxed

-- Tagless final doesn't seem to work well, because we need to gather
-- @Delta@ while doing @DualDelta@ operations, but evaluate on concrete
-- vectors that correspond to only the second component of dual numbers.
-- Also, initial encoding gives us full control over
-- when to evaluate. With final, we'd need to be careful
-- about laziness to ensure the optimal evaluation order is chosen
-- (whatever it is for a given differentiated program).
data Delta r =
    Zero
  | Scale r (Delta r)
  | Add (Delta r) (Delta r)
  | Var DeltaId

newtype DeltaId = DeltaId Int
  deriving (Show, Eq, Ord, Enum)

-- This can't be environment in a Reader, because subtrees add their own
-- identifiers for sharing, instead of parents naming their subtrees.
-- This must be the "evaluate Let backwards" from SPJ's talk.
-- This and the need to control evaluation order contribute to
-- the difficulty of applying any HOAS concept instead of the monad
-- with bindings accumulated in state.
-- Note that each variable is created only once, but the subexpression
-- it's a part of can get duplicated grossly.
data DeltaState r = DeltaState
  { deltaCounter  :: DeltaId
  , deltaBindings :: [Delta r]
  }

buildVector :: forall s v r. (Eq r, Num r, VM.MVector (V.Mutable v) r)
            => Int -> DeltaState r -> Delta r
            -> ST s (V.Mutable v s r)
buildVector dim st d0 = do
  let DeltaId storeSize = deltaCounter st
  store <- VM.replicate storeSize 0
  let eval :: r -> Delta r -> ST s ()
      eval !r = \case
        Zero -> return ()
        Scale k d -> eval (k * r) d
        Add d1 d2 -> eval r d1 >> eval r d2
        Var (DeltaId i) -> VM.modify store (+ r) i
  eval 1 d0  -- dt is 1 or hardwired in f
  let evalUnlessZero :: DeltaId -> Delta r -> ST s DeltaId
      evalUnlessZero delta@(DeltaId !i) d = do
        r <- store `VM.read` i
        when (r /= 0) $  -- we init with exactly 0 above so the comparison is OK
          eval r d
        return $! pred delta
  minusOne <- foldM evalUnlessZero (DeltaId $ pred storeSize) (deltaBindings st)
  let _A = assert (minusOne == DeltaId (-1)) ()
  return $! VM.slice 0 dim store

evalBindingsV :: (Eq r, Num r, V.Vector v r)
              => VecDualDelta i -> DeltaState r -> Delta r -> v r
evalBindingsV ds st d0 = V.create $ buildVector (V.length $ snd ds) st d0

newtype DeltaMonad r a = DeltaMonad
  { runDeltaMonad :: State (DeltaState r) a }
  deriving (Monad, Functor, Applicative)

-- TODO: when varied benchmarks are available, check if returning v always,
-- except for @Add@, is faster. Probably @Zero@ and @Var@ appear too rarely
-- to matter if @Scale@ turns out to require bindings.
dlet :: Delta r -> DeltaMonad r (Delta r)
dlet v = DeltaMonad $ do
  i <- gets deltaCounter
  modify $ \s ->
    s { deltaCounter = succ i
      , deltaBindings = v : deltaBindings s
      }
  return $! Var i

data DualDelta r = D r (Delta r)

type VecDualDelta r = (Domain r, Data.Vector.Vector (Delta r))

var :: Data.Vector.Unboxed.Unbox r
    => Int -> VecDualDelta r -> DualDelta r
var i (vValue, vVar) = D (vValue V.! i) (vVar V.! i)

generalDf :: (domain -> (VecDualDelta r, Int))
          -> (VecDualDelta r -> DeltaState r -> Delta r -> domain')
          -> (VecDualDelta r -> DeltaMonad r (DualDelta r))
          -> domain
          -> (domain', r)
{-# INLINE generalDf #-}
generalDf initVars evalBindings f deltaInput =
  let (ds, dim) = initVars deltaInput
      initialState = DeltaState
        { deltaCounter = DeltaId dim
        , deltaBindings = []
        }
      (D value d, st) = runState (runDeltaMonad (f ds)) initialState
      gradient = evalBindings ds st d
  in (gradient, value)

type Domain r = Data.Vector.Unboxed.Vector r

type Domain' r = Domain r

df :: forall r . (Eq r, Num r, Data.Vector.Unboxed.Unbox r)
   => (VecDualDelta r -> DeltaMonad r (DualDelta r))
   -> Domain r
   -> (Domain' r, r)
df =
  let initVars :: Domain r -> (VecDualDelta r, Int)
      initVars deltaInput =
        let dim = V.length deltaInput
        in ((deltaInput, V.generate dim (Var . DeltaId)), dim)
  in generalDf initVars evalBindingsV

gradDesc :: forall r . (Eq r, Num r, Data.Vector.Unboxed.Unbox r)
         => r
         -> (VecDualDelta r -> DeltaMonad r (DualDelta r))
         -> Int
         -> Domain r
         -> Domain' r
gradDesc gamma f n0 params0 = go n0 params0 where
  dim = V.length params0
  -- Pre-allocating the vars once, vs gradually allocating on the spot in each
  -- gradient computation, initially incurs overhead (looking up in a vector),
  -- but pays off greatly as soon as the working set doesn't fit in any cache
  -- and so allocations are made in RAM.
  vVar = V.generate dim (Var . DeltaId)
  go :: Int -> Domain r -> Domain' r
  go 0 !params = params
  go n params =
    let initVars :: (VecDualDelta r, Int)
        initVars = ((params, vVar), dim)
        gradient = fst $ generalDf (const initVars) evalBindingsV f params
        paramsNew = V.zipWith (\i r -> i - gamma * r) params gradient
    in go (pred n) paramsNew

-- Based on @gradientDescent@ from package @ad@ which is in turn based
-- on the one from the VLAD compiler.
gradDescSmart :: forall r . (Ord r, Fractional r, Data.Vector.Unboxed.Unbox r)
              => (VecDualDelta r -> DeltaMonad r (DualDelta r))
              -> Int
              -> Domain r
              -> (Domain' r, r)
gradDescSmart f n0 params0 = go n0 params0 0.1 gradient0 value0 0 where
  dim = V.length params0
  vVar = V.generate dim (Var . DeltaId)
  initVars0 :: (VecDualDelta r, Int)
  initVars0 = ((params0, vVar), dim)
  (gradient0, value0) = generalDf (const initVars0) evalBindingsV f params0
  go :: Int -> Domain r -> r -> Domain r -> r -> Int -> (Domain' r, r)
  go 0 !params !gamma _gradientPrev _valuePrev !_i = (params, gamma)
  go _ params 0 _ _ _ = (params, 0)
  go n params gamma gradientPrev valuePrev i =
    -- The trick is that we use the previous gradient here,
    -- and the new gradient is only computed by accident together
    -- with the new value that is needed now to revert if we overshoot.
    let paramsNew = V.zipWith (\p r -> p - gamma * r) params gradientPrev
        initVars = ((paramsNew, vVar), dim)
        (gradient, value) = generalDf (const initVars) evalBindingsV f paramsNew
    in if | V.all (== 0) gradientPrev -> (params, gamma)
          | value > valuePrev ->
              go n params (gamma / 2) gradientPrev valuePrev 0  -- overshot
          | i == 10 -> go (pred n) paramsNew (gamma * 2) gradient value 0
          | otherwise -> go (pred n) paramsNew gamma gradient value (i + 1)

(*\) :: Num r => DualDelta r -> DualDelta r -> DeltaMonad r (DualDelta r)
(*\) (D u u') (D v v') = do
  d <- dlet $ Add (Scale v u') (Scale u v')
  return $! D (u * v) d

(+\) :: Num r => DualDelta r -> DualDelta r -> DeltaMonad r (DualDelta r)
(+\) (D u u') (D v v') = do
  d <- dlet $ Add u' v'
  return $! D (u + v) d

(-\) :: Num r => DualDelta r -> DualDelta r -> DeltaMonad r (DualDelta r)
(-\) (D u u') (D v v') = do
  d <- dlet $ Add u' (Scale (-1) v')
  return $! D (u - v) d

(**\) :: Floating r
      => DualDelta r -> DualDelta r -> DeltaMonad r (DualDelta r)
(**\) (D u u') (D v v') = do
  d <- dlet $ Add (Scale (v * (u ** (v - 1))) u')
                  (Scale ((u ** v) * log u) v')
  return $! D (u ** v) d

scalar :: r -> DualDelta r
scalar k = D k Zero

scale :: Num r => r -> DualDelta r -> DeltaMonad r (DualDelta r)
scale k (D u u') = do
  d <- dlet $ Scale k u'
  return $! D (k * u) d

tanhAct :: Floating r => DualDelta r -> DeltaMonad r (DualDelta r)
tanhAct (D u u') = do
  let y = tanh u
  d <- dlet $ Scale (1 - y * y) u'
  return $! D y d

reluAct :: (Num r, Ord r) => DualDelta r -> DeltaMonad r (DualDelta r)
reluAct (D u u') = do
  d <- dlet $ Scale (if u > 0 then 1 else 0) u'
  return $! D (max 0 u) d

-- | Compute the output of a neuron, without applying activation function,
-- from trainable inputs in @xs@ and parameters (the bias and weights)
-- at @vec@ starting at @offset@. Useful for neurons in the middle
-- of the network, receiving inputs from other neurons.
sumTrainableInputs :: forall r. (Num r, Data.Vector.Unboxed.Unbox r)
                   => Data.Vector.Vector (DualDelta r)
                   -> Int
                   -> VecDualDelta r
                   -> DeltaMonad r (DualDelta r)
sumTrainableInputs xs offset vec = do
  let bias = var offset vec
      f :: DualDelta r -> Int -> DualDelta r -> DualDelta r
      f (D acc acc') i (D u u') =
        let (D v v') = var (offset + 1 + i) vec
        in D (acc + u * v) (Add acc' (Add (Scale v u') (Scale u v')))
      D xsum xsum' = V.ifoldl' f bias xs
  d <- dlet xsum'
  return $! D xsum d

-- | Compute the output of a neuron, without applying activation function,
-- from constant data in @xs@ and parameters (the bias and weights)
-- at @vec@ starting at @offset@. Useful for neurons at the bottom
-- of the network, tasked with ingesting the data.
sumConstantData :: forall r. (Num r, Data.Vector.Unboxed.Unbox r)
                => Data.Vector.Unboxed.Vector r
                -> Int
                -> VecDualDelta r
                -> DeltaMonad r (DualDelta r)
sumConstantData xs offset vec = do
  let bias = var offset vec
      f :: DualDelta r -> Int -> r -> DualDelta r
      f (D acc acc') i r =
        let (D v v') = var (offset + 1 + i) vec
        in D (acc + r * v) (Add acc' (Scale r v'))
      D xsum xsum' = V.ifoldl' f bias xs
  d <- dlet xsum'
  return $! D xsum d

lossSquared :: Num r => r -> DualDelta r -> DeltaMonad r (DualDelta r)
lossSquared targ res = do
  diff <- res -\ scalar targ
  diff *\ diff

type DualDeltaF = DualDelta Float

type VecDualDeltaF = VecDualDelta Float

type DeltaMonadF = DeltaMonad Float

type DualDeltaD = DualDelta Double

type VecDualDeltaD = VecDualDelta Double

type DeltaMonadD = DeltaMonad Double







-- recursion and recursive types
-- selective fusion of delta (for individual subfunctions: pre-computing,
--   inlining results and simplifying delta-expressions; the usual inlining
--   considerations apply)
-- checkpointing (limiting space usage?)
