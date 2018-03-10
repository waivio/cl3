
```haskell
--          CCCCCCCCCC      llllllllll                     3333333333
--      CCCCCCCCCCCCCCCCCC   llllllllll                333333333333333333
--    CCCCCCC      CCCCCCCCCC llllllllll            3333333333       333333
--   CCCCC               CCC   llllllllll            333         ##   33333
--  CCCCCC                      llllllllll                       #    33333
--  CCCCC                        llllllllll                          33333
--  CCCCC                         llllllllll          /      @    333333
--  CCCCC                          llllllllll        /|  +         33333
--  CCCCC                          lllllllllll       \|  +         33333
--  CCCCC                         lllllllllllll       \      @    333333
--  CCCCC                        lllllll lllllll                     33333
--  CCCCCC                      lllllll   lllllll                #    33333
--   CCCCC               CCC   lllllll     lllllll   333         ##   33333
--    CCCCCCC      CCCCCCCCCC lllllll       lllllll 3333333333       333333
--      CCCCCCCCCCCCCCCCCC   lllllll         lllllll   333333333333333333
--          CCCCCCCCCC      lllllll           lllllll      3333333333
```



# Cl3
Cl3 is a Haskell Library implementing standard functions for the [Algebra of Physical Space](https://en.wikipedia.org/wiki/Algebra_of_physical_space) Cl(3,0)

The goal of the Cl3 library is to provide a specialized, safe, high performance, Algebra of Physical Space implementation.
This library is suitable for physics simulations.  The library integrates into Haskell's standard prelude and has few dependencies.
The library uses a GADT Syntax to specialize to specific graded elements in the Algebra of Physical Space.


# GADT Syntax Interface
The constructors are specialized to single and double grade combinations and the general case of APS.
Using the specialized constructors helps the compiler to compile to code similar to that you would hand write.
The constructors follow the following conventions for basis.

```haskell
scalar = R e0
vector = V3 e1 e2 e3
bivector = BV e23 e31 e12
trivectorPseudoScalar = I e123
paravector = PV e0 e1 e2 e3
quarternion = H e0 e23 e31 e12
complex = C e0 e123
biparavector = BPV e1 e2 e3 e23 e31 e12
oddparavector = ODD e1 e2 e3 e123
triparavector = TPV e23 e31 e12 e123
aps = APS e0 e1 e2 e3 e23 e31 e12 e123
```
# Usage
In MATLAB or Octave one can write: `sqrt(-25)` and get `5.0i`

In standard Haskell `sqrt (-25)` will produce `NaN`

But using the Cl3 library `sqrt (-25) :: Cl3` will produce `I 5.0`, and likewise `(I 5.0)^2` will produce `R (-25)`

If the unit imaginary is defined as `i = I 1`, expressions very similar to MATLAB can be formed `1.2 + 2.3*i` will produce `C 1.2 2.3`

Vector addition is also natural, two arbitrary vectors `v1 = V3 a1 a2 a3` and `v2 = V3 b1 b2 b3` can be added `v1 + v2` and scaled `2*(v1+v2)`

The dot product (inner product) of two arbitrary vectors is `toR $ v1 * v2`, that is the scalar part of the geometric product of two vectors.

The cross product is the Hodge Dual of the wedge product (outer product) `-i * toBV (v1*v2)`

The multiplication of two unit vectors is related to the rotor rotating from `u_from` to `u_to` like so `rot = sqrt $ u_to * u_from`

Any arbitrary vector can be rotated by a rotor with the equation of `v' = rot * v * dag rot`

Rotors can also be formed with an axis unit vector `u` and real scalar angle `theta` in units of radians, it produces the versor (unit quaternion) `rot = exp $ (-i/2) * theta * u`

For special relativity with the velocity vector `v` and speed of light scalar `c`:
* Beta is `beta = v / c`
* Rapidity is `rapidity = atanh beta`
* Gamma is `gamma = cosh rapidity`
* Composition of velocities is simply adding the two rapidities and converting back to velocity
* Proper Velocity is `w = c * sinh rapidity` or `w = gamma * v`
* Four Velocity is a paravector `u = exp rapidity` where the real scalar part is `gamma * c` and the vector part is `w / c`
* The Boost is `boost = exp $ rapidity / 2`

# APS Basis
Where __e0__ is the scalar basis frequently refered to as "1", in other texts.

__e1__, __e2__, and __e3__ are the vector basis of 3 orthonormal vectors.

__e23__, __e31__, and __e12__ are the bivector basis, these are formed by the outer product of two vector basis. For instance in the case of __e23__, the outer product, or wedge product, is __e2__ /\ __e3__, but because this can be simplified to the geometric product of __e2__ * __e3__ because the scalar part is zero for orthoginal vector basis'.  The geometric product of the two basis vectors is further shortened for brevity to __e23__.

__e123__ is the trivector basis, and is formed by the wedge product of __e1__ /\ __e2__ /\ __e3__, and likewise shortened to __e123__


# Multiplication of the basis elements
The basis vectors multiply with the following multiplication table:

|    Mult  |   e0 |   e1 |   e2 |   e3 |  e23 |  e31 |  e12 | e123 |
|:--------:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|
|   __e0__ |   e0 |   e1 |   e2 |   e3 |  e23 |  e31 |  e12 | e123 |
|   __e1__ |   e1 |   e0 |  e12 | -e31 | e123 |  -e3 |   e2 |  e23 |
|   __e2__ |   e2 | -e12 |   e0 |  e23 |   e3 | e123 |  -e1 |  e31 |
|   __e3__ |   e3 |  e31 | -e23 |   e0 |  -e2 |   e1 | e123 |  e12 |
|  __e23__ |  e23 | e123 |  -e3 |   e2 |  -e0 | -e12 |  e31 |  -e1 |
|  __e31__ |  e31 |   e3 | e123 |  -e1 |  e12 |  -e0 | -e23 |  -e2 |
|  __e12__ |  e12 |  -e2 |   e1 | e123 | -e31 |  e23 |  -e0 |  -e3 |
| __e123__ | e123 |  e23 |  e31 |  e12 |  -e1 |  -e2 |  -e3 |  -e0 |


# Multiplication of the GADT Syntax Constructors
The grade specialized type constructors multiply with the following multiplication table:

| Mult    |   R |  V3 |  BV |   I |  PV |   H |   C | BPV | ODD | TPV | APS |
|:-------:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
|   __R__ |   R |  V3 |  BV |   I |  PV |   H |   C | BPV | ODD | TPV | APS |
|  __V3__ |  V3 |   H | ODD |  BV | APS | ODD | BPV | APS | ODD | APS | APS |
|  __BV__ |  BV | ODD |   H |  V3 | APS |   H | BPV | APS | ODD | APS | APS |
|   __I__ |   I |  BV |  V3 |   R | TPV | ODD |   C | BPV |   H |  PV | APS |
|  __PV__ |  PV | APS | APS | TPV | APS | APS | APS | APS | APS | APS | APS |
|   __H__ |   H | ODD |   H | ODD | APS |   H | APS | APS | ODD | APS | APS |
|   __C__ |   C | BPV | BPV |   C | APS | APS |   C | BPV | APS | APS | APS |
| __BPV__ | BPV | APS | ODD | BPV | APS | APS | BPV | APS | APS | APS | APS |
| __ODD__ | ODD | ODD | TPV |   H | APS | ODD | APS | APS |   H | APS | APS |
| __TPV__ | TPV | APS | APS |  PV | APS | APS | APS | APS | APS | APS | APS |
| __APS__ | APS | APS | APS | APS | APS | APS | APS | APS | APS | APS | APS |


# Design Philosophy
The design space for Clifford Algebra libraries was explored quite a bit before the development of this library.  Initially the isomorphism of APS with 2x2 Complex Matrices was used, this had the draw back that multiplying the scalar 2 * 2 would incur all of the computational cost of multiplying two 2x2 complex matrices.
Then the design was changed to lists that contained the basis' values, but lists are computationally slow and do not produce well optimized code.
Then a single constructor data type for APS was developed, but this had all of the drawbacks of 2x2 complex matrices.
The GADT Syntax version of the library was developed and it showed that it had some promise.
More of the design space was explored, a version of the Cl3 library was developed using Multi-parameter Type Classes and Functional Dependencies, this didn't appear to have much gained over the GADT Syntax interface and it didn't use the standard Prelude classes like Num, Float, etc.  It was also difficult for me to figure out to code a `reduce` function.
So the GADT Syntax design of the Cl3 library was finished and released.

# How does this fit in with the existing Haskell ecosystem?
Cl3 is meant to be a [Linear](https://hackage.haskell.org/package/linear) killer based on Geometric Algebra.  The linear package
consists of many different types that are not easily combinable using the Num Class.

Whereas [clifford](https://hackage.haskell.org/package/clifford) uses [Numeric Prelude](https://hackage.haskell.org/package/numeric-prelude),
Cl3 is an easy to use, high performance, library that is based on the standard Prelude.

Whereas [clif](https://hackage.haskell.org/package/clif) is for symbolic computing using symbolic and numeric computations
with finite and infinite-dimensional Clifford algebras arising from arbitrary bilinear forms.  Cl3 is specialized to the
Algebra of Physical Space, Cl3, using a high performance easy to use GADT Syntax interface.


