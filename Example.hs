module Example where

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

