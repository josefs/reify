# Abstract

Deeply embedded languages are often based on the idea of reifying program syntax to an AST without having access to the source code. It is often convenient for embedded languages to reuse existing interfaces; for example the Num class, which gives support for numeric literals. Likewise, for monadic computations, it is desired to implement the Monad class in order to reuse do-notation and existing monadic combinators. However, the Monad class is usually problematic for embedded languages because the types required for return and bind are too polymorphic.

In this talk, we will present a number of different solutions to the monad reification problem. The first solution is implemented in Feldspar and published in IFL 2012. Two other solutions have been submitted to ICFP 2013, one by our group.

