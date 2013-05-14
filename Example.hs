{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Example where

import qualified Control.Monad.State as State
import Data.IORef

data E a where
  Var :: Int -> E a  -- Not part of syntax; only for reification
  Val :: a -> E a    -- Not part of syntax; only for reification
  NUM :: (Num a, Show a) => a -> E a
  Add :: Num a => E a -> E a -> E a

evalE :: E a -> a
evalE (Val a)   = a
evalE (NUM a)   = a
evalE (Add a b) = evalE a + evalE b

compileE :: E a -> String
compileE (Var v)   = 'v' : show v
compileE (NUM a)   = show a
compileE (Add a b) = "(" ++ compileE a ++ " + " ++ compileE b ++ ")"

-- Num instance works fine
instance (Num a, Show a) => Num (E a) where
  fromInteger = NUM . fromInteger
  (+) = Add

type R a = IORef a

data S a where
  Ret  :: E a -> S a
  Bind :: S a -> (E a -> S b) -> S b  -- Higher-order abstract syntax
  New  :: E a -> S (R a)
  Get  :: E (R a) -> S a
  Set  :: E (R a) -> E a -> S ()

evalS :: S a -> IO a
evalS (Ret a)    = return (evalE a)
evalS (Bind a f) = evalS a >>= \a' -> evalS (f (Val a'))
evalS (New a)    = newIORef (evalE a)
evalS (Get r)    = readIORef (evalE r)
evalS (Set r a)  = writeIORef (evalE r) (evalE a)

fresh = do v <- State.get; State.put (v+1); return v

compileS :: S a -> String
compileS = flip State.evalState 0 . go
  where
    go :: S a -> State.State Int String
    go (Ret a)    = return $ "return " ++ compileE a
    go (Bind a f) = do
        v     <- fresh
        aProg <- go a
        fProg <- go (f (Var v))
        return $ "v" ++ show v ++ " <- " ++ aProg ++ "\n" ++ fProg
    go (New a)    = return $ unwords ["new", compileE a]
    go (Get a)    = return $ unwords ["get", compileE a]
    go (Set r a)  = return $ unwords ["set", compileE r, compileE a]



-- Solution 1

-- Monad instance does not type check
--instance Monad S
--  where
--    return = Ret
--    (>>=)  = Bind

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

eval :: M (E a) -> IO a
eval = evalS . runM

compile :: M (E a) -> String
compile = compileS . runM



-- Examples

prog1 :: M (E Int)
prog1 = do
    a  <- new 4
    b  <- new 5
    a' <- get a
    b' <- get b
    set b (a' + b')
    get b

prog2 :: M (E Int)
prog2 = do
    a <- new 4
    b <- new 5
    c <- do
        a' <- get a
        b' <- get b
        return (a' + b')
    set b c
    get b

test1 = putStrLn $ compile prog1
test2 = eval prog1
test3 = putStrLn $ compile prog2
test4 = eval prog2
