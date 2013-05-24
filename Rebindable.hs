{-# LANGUAGE RebindableSyntax #-}

module Rebindable where

import Prelude hiding (Monad (..))

import Example



----------------------------------------------------------------------
-- Rebindable syntax
----------------------------------------------------------------------

return = Ret
(>>=)  = Bind
fail   = error

m >> n = m >>= \_ -> n

new = New
get = Get
set = Set



----------------------------------------------------------------------
-- Examples
----------------------------------------------------------------------

prog1 :: S Int
prog1 = do
    a  <- new 4
    b  <- new 5
    a' <- get a
    b' <- get b
    set b (a' + b')
    get b

prog2 :: S Int
prog2 = do
    a <- new 4
    b <- new 5
    c <- do
        a' <- get a
        b' <- get b
        return (a' + b')
    set b c
    get b

test1 = putStrLn $ compileS prog1
test2 = putStrLn $ compileS prog2
test3 = evalS prog1
test4 = evalS prog2

-- The structure is reified faithfully (see `test1` and `test2`), but this
-- violates the monad laws...

