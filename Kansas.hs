module Kansas where

import Continuation
  ( E (..)
  , evalE
  , compileE
  , R
  , S (..)
  , evalS
  , compileS
  )



----------------------------------------------------------------------------------------------------
-- "Kansas" solution
----------------------------------------------------------------------------------------------------

-- Sculthorpe, Bracker, Giorgidze, Gill. The Constrained-Monad Problem. Submitted to ICFP 2013.

-- Normal form of constrained monadic expressions
data M a where
  RET  :: a -> M a
  BIND :: S a -> (E a -> M b) -> M b

instance Monad M where
  return         = RET
  RET a    >>= k = k a
  BIND s h >>= k = BIND s (\a -> h a >>= k)

-- * `>>=` does not introduce a `BIND`
-- * Both occurrences of `BIND` bind the same type `E a`
-- * The argument of `k` is polymorphic

-- `BIND` can have arbitrary constraints on the bound value, e.g.
--
--     BIND :: Eq a => T a -> (a -> M b) -> M b
--
-- This way we can implement other constrained monads.

lower :: M (E a) -> S a
lower (RET a)    = Ret a
lower (BIND s k) = Bind s (lower . k)

lift :: S a -> M (E a)
lift s = BIND s RET

-- * `Ret` and `Bind` only introduced by `lower`
-- * All occurrences of `Bind` in lower have the same result type
-- * I.e. can also support monads where result of bind is constrained (e.g. `Set` monad)

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
test2 = putStrLn $ compile prog2
test3 = eval prog1
test4 = eval prog2

