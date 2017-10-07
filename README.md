# cl3
Haskell Library implementing standard functions for the Algebra of Physical Space Cl(3,0)

The goal of the Cl3 library is to provide a specialized, safe, high performance, Algebra of Physical Space implementaiton.
This library is suitable for physics simulations.  The library integrates into Haskell's standard prelude and has few dependancies.
The library uses a GADT to specialize to specific graded elements in the Algebra of Physical Space.

## How does this fit in with the existing Haskell ecosystem?
Cl3 is meant to be a [Linear] (https://hackage.haskell.org/package/linear) killer based on Geometric Algebra.

Whereas [clifford] (https://hackage.haskell.org/package/clifford) uses [Numeric Prelude] (https://hackage.haskell.org/package/numeric-prelude), 
Cl3 is an easy to use, high performance, library that is based on the standard Prelude.

Whereas [clif] (https://hackage.haskell.org/package/clif) is for symbolic computing with or symbolic and numeric computations
with finite and infinite-dimensional Clifford algebras arising from arbitrary bilinear forms.  Cl3 is specialized to the
Algebra of Physical Space, Cl3, using a high performance easy to use GADT interface.