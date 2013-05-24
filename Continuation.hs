module Continuation where

import Example



----------------------------------------------------------------------
-- Feldspar solution
----------------------------------------------------------------------

-- Persson, Axelsson, Svenningsson. Generic Monadic Constructs for Embedded
-- Languages. IFL 2012.

-- First published solution to the monad reification problem

data M a = M { unM :: (forall b. (a -> S b) -> S b) }

-- Note, `a` can be any type, but the only result we can get from
-- `M a` is an expression.

instance Monad M where
  return a  = M $ \k -> k a
  M f >>= h = M $ \k -> f (\a -> unM (h a) k)

-- Continuation example
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

lift :: S a -> M (E a)
lift s = M $ \k -> Bind s k

lower :: M (E a) -> S a
lower (M f) = f Ret

-- Note: `Ret`  only introduced by `lower`
--       `Bind` only introduced by `lift`

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



----------------------------------------------------------------------
-- Examples
----------------------------------------------------------------------

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

-- `test1` and `test2` give the same result (normalization)

