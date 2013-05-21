module Continuation where

import qualified Control.Monad.State as State
import Data.IORef



----------------------------------------------------------------------------------------------------
-- Arithmetic expressions
----------------------------------------------------------------------------------------------------

data E a where
  Var :: Int -> E a  -- Variable; syntactic interpretation
  Val :: a -> E a    -- Variable; semantic
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



----------------------------------------------------------------------------------------------------
-- Adding references and monadic combinators
----------------------------------------------------------------------------------------------------

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

-- Monad instance does not type check :(
--instance Monad S
--  where
--    return = Ret
--    (>>=)  = Bind



----------------------------------------------------------------------------------------------------
-- Feldspar solution
----------------------------------------------------------------------------------------------------

-- Persson, Axelsson, Svenningsson. Generic Monadic Constructs for Embedded Languages. IFL 2012.

-- First published solution to the monad reification problem

data M a = M { unM :: (forall b. (a -> S b) -> S b) }

-- Note, `a` can be any type, but the only result we can get from `M a` is an expression.

instance Monad M where
  return a  = M (\k -> k a)
  M f >>= g = M (\k -> f (\a -> unM (g a) k))

ex1 = do
    a <- return 1
    b <- return 2
    return (a+b)

ex1' =
    return 1 >>= \a ->
    return 2 >>= \b ->
    return (a+b)

ex1'' = M
    (\k -> (\k -> k 1) (\aa -> (\a ->
    (\k -> (\k -> k 2) (\ab -> (\b ->
    (\k -> k (a+b))) ab k))) aa k))

ex1''' = M (\k -> k (1+2))  --  == return (1+2)

lower :: M (E a) -> S a
lower (M f) = f Ret

lift :: S a -> M (E a)
lift s = M (\k -> Bind s k)

-- Note: `Bind` only introduced by `lower`
--       `Ret`  only introduced by `lift`

new :: E a -> M (E (R a))
new a = lift (New a)

get :: E (R a) -> M (E a)
get r = lift (Get r)

set :: E (R a) -> E a -> M (E ())
set r a = lift (Set r a)

eval :: M (E a) -> IO a
eval = evalS . lower

compile :: M (E a) -> String
compile = compileS . lower



----------------------------------------------------------------------------------------------------
-- Examples
----------------------------------------------------------------------------------------------------

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
test2 = putStrLn $ compile prog2
test3 = eval prog1
test4 = eval prog2

