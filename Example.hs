{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
module Example where

import Data.IORef

newtype R a = R { unR :: IORef a }

data S a where
  Ret  :: E a -> S a
  Bind :: S a -> (E a -> S b) -> S b
  New  :: E a -> S (R a)
  Get  :: E (R a) -> S a
  Set  :: E (R a) -> E a -> S ()

data E a where
  Int :: Int -> E Int
  Add :: E a -> E a -> E a'

-- Solution 1

data M a = M { unM :: (forall b. (a -> S b) -> S b) }

instance Monad M where
  return a = M (\k -> k a)
  M f >>= g = M (\k -> f (\a -> unM (g a) k))

new :: E a -> M (E (R a))
new a = M (\k -> Bind (New a) k)

get :: E (R a) -> M (E a)
get r = M (\k -> Bind (Get r) k)

set :: E (R a) -> E a -> M (E ())
set r a = M (\k -> Bind (Set r a) k)

runM :: M (E a) -> S a
runM (M f) = f Ret
