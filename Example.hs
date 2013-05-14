{-# LANGUAGE GADTs #-}
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

