{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}


#ifndef O_LIQUID
-- Turn off Safe Haskell language extension due to liquid-base re-exports
{-# LANGUAGE Safe #-}
#endif

#if __GLASGOW_HASKELL__ == 810
-- Work around to fix GHC Issue #15304, issue popped up again in GHC 8.10, it should be fixed in GHC 8.12
-- This code is meant to reproduce MR 2608 for GHC 8.10
{-# OPTIONS_GHC -funfolding-keeness-factor=1 -funfolding-use-threshold=80 #-}
#endif



--------------------------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2017-2022 Nathan Waivio
-- License     :  BSD3
-- Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
-- Stability   :  Stable
-- Portability :  unportable
--
-- Library implementing standard functions for the <https://en.wikipedia.org/wiki/Algebra_of_physical_space Algebra of Physical Space> Cl(3,0)
-- 
---------------------------------------------------------------------------------------------


module Algebra.Geometric.Cl3
(-- * The type for the Algebra of Physical Space
 Cl3(..),
 -- * Clifford Conjugate and Complex Conjugate
 bar, dag,
 -- * The littlest singular value
 lsv,
 -- * Constructor Selectors - For optimizing and simplifying calculations
 toR, toV3, toBV, toI,
 toPV, toH, toC,
 toBPV, toODD, toTPV,
 toAPS,
 -- * Pretty Printing for use with Octave
 showOctave,
 -- * Eliminate grades that are less than 'tol' to use a simpler Constructor
 reduce, tol,
#ifndef O_NO_STORABLE
 -- * Compact Storable types for the Cl3 Constructors with smart constructors
 Cl3_R, toCl3_R, fromCl3_R,
 Cl3_V3, toCl3_V3, fromCl3_V3,
 Cl3_BV, toCl3_BV, fromCl3_BV,
 Cl3_I, toCl3_I, fromCl3_I,
 Cl3_PV, toCl3_PV, fromCl3_PV,
 Cl3_H, toCl3_H, fromCl3_H,
 Cl3_C, toCl3_C, fromCl3_C,
 Cl3_BPV, toCl3_BPV, fromCl3_BPV,
 Cl3_ODD, toCl3_ODD, fromCl3_ODD,
 Cl3_TPV, toCl3_TPV, fromCl3_TPV,
 Cl3_APS, toCl3_APS, fromCl3_APS,
#endif
#ifndef O_NO_RANDOM
 -- * Random Instances
 randR, rangeR,
 randV3, rangeV3,
 randBV, rangeBV,
 randI, rangeI,
 randPV, rangePV,
 randH, rangeH,
 randC, rangeC,
 randBPV, rangeBPV,
 randODD, rangeODD,
 randTPV, rangeTPV,
 randAPS, rangeAPS,
 randUnitV3,
 randProjector,
 randNilpotent,
 randUnitary,
#endif
 -- * Helpful Functions
 eigvals, hasNilpotent,
 spectraldcmp, project,
 mIx, timesI,
 abssignum
) where

#ifndef O_NO_DERIVED
import Data.Data (Typeable, Data)
import GHC.Generics (Generic)
import Text.Read (Read,readPrec)
#endif

import Control.DeepSeq (NFData,rnf)

#ifndef O_NO_STORABLE
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
#endif

#ifndef O_NO_RANDOM
import System.Random (RandomGen, Random, randomR, random)
#endif


-- | Cl3 provides specialized constructors for sub-algebras and other geometric objects
-- contained in the algebra.  Cl(3,0), abbreviated to Cl3, is a Geometric Algebra
-- of 3 dimensional space known as the Algebra of Physical Space (APS).  Geometric Algebras are Real
-- Clifford Algebras, double precision floats are used to approximate real numbers in this
-- library.  Single and Double grade combinations are specialized using algebraic datatypes
-- and live within the APS.
--
--   * 'R' is the constructor for the Real Scalar Sub-algebra Grade-0
--
--   * 'V3' is the Three Dimensional Real Vector constructor Grade-1
--
--   * 'BV' is the Bivector constructor Grade-2 an Imaginary Three Dimensional Vector
--
--   * 'I' is the Imaginary constructor Grade-3 and is the Pseudo-Scalar for APS
--
--   * 'PV' is the Paravector constructor with Grade-0 and Grade-1 elements, a Real Scalar plus Vector, (R + V3)
--
--   * 'H' is the Quaternion constructor it is the Even Sub-algebra with Grade-0 and Grade-2 elements, a Real Scalar plus Bivector, (R + BV)
--
--   * 'C' is the Complex constructor it is the Scalar Sub-algebra with Grade-0 and Grade-3 elements, a Real Scalar plus Imaginar Scalar, (R + I)
--
--   * 'BPV' is the Biparavector constructor with Grade-1 and Grade-2 elements, a Real Vector plus Bivector, (V3 + BV)
--
--   * 'ODD' is the Odd constructor with Grade-1 and Grade-3 elements, a Vector plus Imaginary Scalar, (V3 + I)
--
--   * 'TPV' is the Triparavector constructor with Grade-2 and Grade-3 elements, a Bivector plus Imaginary, (BV + I)
--
--   * 'APS' is the constructor for an element in the Algebra of Physical Space with Grade-0 through Grade-3 elements
--
data Cl3 where
  R   :: !Double -> Cl3 -- Real Scalar Sub-algebra
  V3  :: !Double -> !Double -> !Double -> Cl3 -- Three Dimensional Vectors
  BV  :: !Double -> !Double -> !Double -> Cl3 -- Bivectors, Imaginary Three Dimenstional Vectors
  I   :: !Double -> Cl3 -- Trivector Imaginary Pseudo-Scalar, Imaginary Scalar
  PV  :: !Double -> !Double -> !Double -> !Double -> Cl3 -- Paravector, Real Scalar plus Three Dimensional Real Vector, (R + V3)
  H   :: !Double -> !Double -> !Double -> !Double -> Cl3 -- Quaternion Even Sub-algebra, Real Scalar plus Bivector, (R + BV)
  C   :: !Double -> !Double -> Cl3 -- Complex Sub-algebra, Real Scalar plus Imaginary Scalar, (R + I)
  BPV :: !Double -> !Double -> !Double -> !Double -> !Double -> !Double -> Cl3 -- Biparavector, Vector plus Bivector, (V3 + BV)
  ODD :: !Double -> !Double -> !Double -> !Double -> Cl3 -- Odd, Vector plus Imaginary, (V3 + I)
  TPV :: !Double -> !Double -> !Double -> !Double -> Cl3 -- Triparavector, Bivector plus Imaginary Scalar, (BV + I)
  APS :: !Double -> !Double -> !Double -> !Double -> !Double -> !Double -> !Double -> !Double -> Cl3 -- Algebra of Physical Space
#ifndef O_NO_DERIVED
    deriving (Show, Read, Typeable, Data, Generic)

#else

-- | In case we don't derive Show, provide 'showOctave' as the Show instance
instance Show Cl3 where
  show = showOctave

#endif


-- | Cl3 can be reduced to a normal form.
instance NFData Cl3 where
  rnf !_ = ()


-- |'showOctave' for useful for debug purposes.
-- The additional octave definition is needed:  
-- 
-- > e0 = [1,0;0,1]; e1=[0,1;1,0]; e2=[0,-i;i,0]; e3=[1,0;0,-1];
--
-- This allows one to take advantage of the isomorphism between Cl3 and M(2,C)
showOctave :: Cl3 -> String
showOctave (R a0) = show a0 ++ "*e0"
showOctave (V3 a1 a2 a3) = show a1 ++ "*e1 + " ++ show a2 ++ "*e2 + " ++ show a3 ++ "*e3"
showOctave (BV a23 a31 a12) = show a23 ++ "i*e1 + " ++ show a31 ++ "i*e2 + " ++ show a12 ++ "i*e3"
showOctave (I a123) = show a123 ++ "i*e0"
showOctave (PV a0 a1 a2 a3) = show a0 ++ "*e0 + " ++ show a1 ++ "*e1 + " ++ show a2 ++ "*e2 + " ++ show a3 ++ "*e3"
showOctave (H a0 a23 a31 a12) = show a0 ++ "*e0 + " ++ show a23 ++ "i*e1 + " ++ show a31 ++ "i*e2 + " ++ show a12 ++ "i*e3"
showOctave (C a0 a123) = show a0 ++ "*e0 + " ++ show a123 ++ "i*e0"
showOctave (BPV a1 a2 a3 a23 a31 a12) = show a1 ++ "*e1 + " ++ show a2 ++ "*e2 + " ++ show a3 ++ "*e3 + " ++
                                        show a23 ++ "i*e1 + " ++ show a31 ++ "i*e2 + " ++ show a12 ++ "i*e3"
showOctave (ODD a1 a2 a3 a123) = show a1 ++ "*e1 + " ++ show a2 ++ "*e2 + " ++ show a3 ++ "*e3 + " ++ show a123 ++ "i*e0"
showOctave (TPV a23 a31 a12 a123) = show a23 ++ "i*e1 + " ++ show a31 ++ "i*e2 + " ++ show a12 ++ "i*e3 + " ++ show a123 ++ "i*e0"
showOctave (APS a0 a1 a2 a3 a23 a31 a12 a123) = show a0 ++ "*e0 + " ++ show a1 ++ "*e1 + " ++ show a2 ++ "*e2 + " ++ show a3 ++ "*e3 + " ++
                                                show a23 ++ "i*e1 + " ++ show a31 ++ "i*e2 + " ++ show a12 ++ "i*e3 + " ++ show a123 ++ "i*e0"


-- |Cl(3,0) has the property of equivalence.  "Eq" is "True" when all of the grade elements are equivalent.
instance Eq Cl3 where
  (R a0) == (R b0) = a0 == b0

  (R a0) == (V3 b1 b2 b3) = a0 == 0 && b1 == 0 && b2 == 0 && b3 == 0
  (R a0) == (BV b23 b31 b12) = a0 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (R a0) == (I b123) = a0 == 0 && b123 == 0
  (R a0) == (PV b0 b1 b2 b3) = a0 == b0 && b1 == 0 && b2 == 0 && b3 == 0
  (R a0) == (H b0 b23 b31 b12) = a0 == b0 && b23 == 0 && b31 == 0 && b12 == 0
  (R a0) == (C b0 b123) = a0 == b0 && b123 == 0
  (R a0) == (BPV b1 b2 b3 b23 b31 b12) = a0 == 0 && b1 == 0 && b2 == 0 && b3 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (R a0) == (ODD b1 b2 b3 b123) = a0 == 0 && b1 == 0 && b2 == 0 && b3 == 0 && b123 == 0
  (R a0) == (TPV b23 b31 b12 b123) = a0 == 0 && b23 == 0 && b31 == 0 && b12 == 0 && b123 == 0
  (R a0) == (APS b0 b1 b2 b3 b23 b31 b12 b123) = a0 == b0 && b1 == 0 && b2 == 0 && b3 == 0 && b23 == 0 && b31 == 0 && b12 == 0 && b123 == 0

  (V3 a1 a2 a3) == (R b0) = a1 == 0 && a2 == 0 && a3 == 0 && b0 == 0
  (BV a23 a31 a12) == (R b0) = a23 == 0 && a31 == 0 && a12 == 0 && b0 == 0
  (I a123) == (R b0) = a123 == 0 && b0 == 0
  (PV a0 a1 a2 a3) == (R b0) = a0 == b0 && a1 == 0 && a2 == 0 && a3 == 0
  (H a0 a23 a31 a12) == (R b0) = a0 == b0 && a23 == 0 && a31 == 0 && a12 == 0
  (C a0 a123) == (R b0) = a0 == b0 && a123 == 0
  (BPV a1 a2 a3 a23 a31 a12) == (R b0) = a1 == 0 && a2 == 0 && a3 == 0 && a23 == 0 && a31 == 0 && a12 == 0 && b0 == 0
  (ODD a1 a2 a3 a123) == (R b0) = a1 == 0 && a2 == 0 && a3 == 0 && a123 == 0 && b0 == 0
  (TPV a23 a31 a12 a123) == (R b0) = a23 == 0 && a31 == 0 && a12 == 0 && a123 == 0 && b0 == 0
  (APS a0 a1 a2 a3 a23 a31 a12 a123) == (R b0) = a0 == b0 && a1 == 0 && a2 == 0 && a3 == 0 && a23 == 0 && a31 == 0 && a12 == 0 && a123 == 0

  (V3 a1 a2 a3) == (V3 b1 b2 b3) = a1 == b1 && a2 == b2 && a3 == b3

  (V3 a1 a2 a3) == (BV b23 b31 b12) = a1 == 0 && a2 == 0 && a3 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (V3 a1 a2 a3) == (I b123) = a1 == 0 && a2 == 0 && a3 == 0 && b123 == 0
  (V3 a1 a2 a3) == (PV b0 b1 b2 b3) = a1 == b1 && a2 == b2 && a3 == b3 && b0 == 0
  (V3 a1 a2 a3) == (H b0 b23 b31 b12) = a1 == 0 && a2 == 0 && a3 == 0 && b0 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (V3 a1 a2 a3) == (C b0 b123) = a1 == 0 && a2 == 0 && a3 == 0 && b0 == 0 && b123 == 0
  (V3 a1 a2 a3) == (BPV b1 b2 b3 b23 b31 b12) = a1 == b1 && a2 == b2 && a3 == b3 && b23 == 0 && b31 == 0 && b12 == 0
  (V3 a1 a2 a3) == (ODD b1 b2 b3 b123) = a1 == b1 && a2 == b2 && a3 == b3 && b123 == 0
  (V3 a1 a2 a3) == (TPV b23 b31 b12 b123) = a1 == 0 && a2 == 0 && a3 == 0 && b23 == 0 && b31 == 0 && b12 == 0 && b123 == 0
  (V3 a1 a2 a3) == (APS b0 b1 b2 b3 b23 b31 b12 b123) = a1 == b1 && a2 == b2 && a3 == b3 && b0 == 0 && b23 == 0 && b31 == 0 && b12 == 0 && b123 == 0

  (BV a23 a31 a12) == (V3 b1 b2 b3) = a23 == 0 && a31 == 0 && a12 == 0 && b1 == 0 && b2 == 0 && b3 == 0
  (I a123) == (V3 b1 b2 b3) = a123 == 0 && b1 == 0 && b2 == 0 && b3 == 0
  (PV a0 a1 a2 a3) == (V3 b1 b2 b3) = a0 == 0 && a1 == b1 && a2 == b2 && a3 == b3
  (H a0 a23 a31 a12) == (V3 b1 b2 b3) = a0 == 0 && a23 == 0 && a31 == 0 && a12 == 0 && b1 == 0 && b2 == 0 && b3 == 0
  (C a0 a123) == (V3 b1 b2 b3) = a0 == 0 && a123 == 0 && b1 == 0 && b2 == 0 && b3 == 0
  (BPV a1 a2 a3 a23 a31 a12) == (V3 b1 b2 b3) = a1 == b1 && a2 == b2 && a3 == b3 && a23 == 0 && a31 == 0 && a12 == 0
  (ODD a1 a2 a3 a123) == (V3 b1 b2 b3) = a1 == b1 && a2 == b2 && a3 == b3 && a123 == 0
  (TPV a23 a31 a12 a123) == (V3 b1 b2 b3) = b1 == 0 && b2 == 0 && b3 == 0 && a23 == 0 && a31 == 0 && a12 == 0 && a123 == 0
  (APS a0 a1 a2 a3 a23 a31 a12 a123) == (V3 b1 b2 b3) = a0 == 0 && a1 == b1 && a2 == b2 && a3 == b3 && a23 == 0 && a31 == 0 && a12 == 0 && a123 == 0

  (BV a23 a31 a12) == (BV b23 b31 b12) = a23 == b23 && a31 == b31 && a12 == b12

  (BV a23 a31 a12) == (I b123) = a23 == 0 && a31 == 0 && a12 == 0 && b123 == 0
  (BV a23 a31 a12) == (PV b0 b1 b2 b3) = a23 == 0 && a31 == 0 && a12 == 0 && b0 == 0 && b1 == 0 && b2 == 0 && b3 == 0
  (BV a23 a31 a12) == (H b0 b23 b31 b12) = a23 == b23 && a31 == b31 && a12 == b12 && b0 == 0
  (BV a23 a31 a12) == (C b0 b123) = a23 == 0 && a31 == 0 && a12 == 0 && b0 == 0 && b123 == 0
  (BV a23 a31 a12) == (BPV b1 b2 b3 b23 b31 b12) = a23 == b23 && a31 == b31 && a12 == b12 && b1 == 0 && b2 == 0 && b3 == 0
  (BV a23 a31 a12) == (ODD b1 b2 b3 b123) = a23 == 0 && a31 == 0 && a12 == 0 && b1 == 0 && b2 == 0 && b3 == 0 && b123 == 0
  (BV a23 a31 a12) == (TPV b23 b31 b12 b123) = a23 == b23 && a31 == b31 && a12 == b12 && b123 == 0
  (BV a23 a31 a12) == (APS b0 b1 b2 b3 b23 b31 b12 b123) = a23 == b23 && a31 == b31 && a12 == b12 && b0 == 0 && b1 == 0 && b2 == 0 && b3 == 0 && b123 == 0

  (I a123) == (BV b23 b31 b12) = a123 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (PV a0 a1 a2 a3) == (BV b23 b31 b12) = a0 == 0 && a1 == 0 && a2 == 0 && a3 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (H a0 a23 a31 a12) == (BV b23 b31 b12) = a0 == 0 && a23 == b23 && a31 == b31 && a12 == b12
  (C a0 a123) == (BV b23 b31 b12) = a0 == 0 && a123 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (BPV a1 a2 a3 a23 a31 a12) == (BV b23 b31 b12) = a1 == 0 && a2 == 0 && a3 == 0 && a23 == b23 && a31 == b31 && a12 == b12
  (ODD a1 a2 a3 a123) == (BV b23 b31 b12) = a1 == 0 && a2 == 0 && a3 == 0 && a123 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (TPV a23 a31 a12 a123) == (BV b23 b31 b12) = a23 == b23 && a31 == b31 && a12 == b12 && a123 == 0
  (APS a0 a1 a2 a3 a23 a31 a12 a123) == (BV b23 b31 b12) = a0 == 0 && a1 == 0 && a2 == 0 && a3 == 0 && a23 == b23 && a31 == b31 && a12 == b12 && a123 == 0

  (I a123) == (I b123) = a123 == b123

  (I a123) == (PV b0 b1 b2 b3) = a123 == 0 && b0 == 0 && b1 == 0 && b2 == 0 && b3 == 0
  (I a123) == (H b0 b23 b31 b12) = a123 == 0 && b0 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (I a123) == (C b0 b123) = a123 == b123 && b0 == 0
  (I a123) == (BPV b1 b2 b3 b23 b31 b12) = a123 == 0 && b1 == 0 && b2 == 0 && b3 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (I a123) == (ODD b1 b2 b3 b123) = a123 == b123 && b1 == 0 && b2 == 0 && b3 == 0
  (I a123) == (TPV b23 b31 b12 b123) = a123 == b123 && b23 == 0 && b31 == 0 && b12 == 0
  (I a123) == (APS b0 b1 b2 b3 b23 b31 b12 b123) = a123 == b123 && b0 == 0 && b1 == 0 && b2 == 0 && b3 == 0 && b23 == 0 && b31 == 0 && b12 == 0

  (PV a0 a1 a2 a3) == (I b123) = b123 == 0 && a0 == 0 && a1 == 0 && a2 == 0 && a3 == 0
  (H a0 a23 a31 a12) == (I b123) = b123 == 0 && a0 == 0 && a23 == 0 && a31 == 0 && a12 == 0
  (C a0 a123) == (I b123) = a123 == b123 && a0 == 0
  (BPV a1 a2 a3 a23 a31 a12) == (I b123) = b123 == 0 && a1 == 0 && a2 == 0 && a3 == 0 && a23 == 0 && a31 == 0 && a12 == 0
  (ODD a1 a2 a3 a123) == (I b123) = a123 == b123 && a1 == 0 && a2 == 0 && a3 == 0
  (TPV a23 a31 a12 a123) == (I b123) = a123 == b123 && a23 == 0 && a31 == 0 && a12 == 0
  (APS a0 a1 a2 a3 a23 a31 a12 a123) == (I b123) = a123 == b123 && a0 == 0 && a1 == 0 && a2 == 0 && a3 == 0 && a23 == 0 && a31 == 0 && a12 == 0

  (PV a0 a1 a2 a3) == (PV b0 b1 b2 b3) = a0 == b0 && a1 == b1 && a2 == b2 && a3 == b3

  (PV a0 a1 a2 a3) == (H b0 b23 b31 b12) = a0 == b0 && a1 == 0 && a2 == 0 && a3 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (PV a0 a1 a2 a3) == (C b0 b123) = a0 == b0 && a1 == 0 && a2 == 0 && a3 == 0 && b123 == 0
  (PV a0 a1 a2 a3) == (BPV b1 b2 b3 b23 b31 b12) = a0 == 0 && a1 == b1 && a2 == b2 && a3 == b3 && b23 == 0 && b31 == 0 && b12 == 0
  (PV a0 a1 a2 a3) == (ODD b1 b2 b3 b123) = a0 == 0 && a1 == b1 && a2 == b2 && a3 == b3 && b123 == 0
  (PV a0 a1 a2 a3) == (TPV b23 b31 b12 b123) = a0 == 0 && a1 == 0 && a2 == 0 && a3 == 0 && b23 == 0 && b31 == 0 && b12 == 0 && b123 == 0
  (PV a0 a1 a2 a3) == (APS b0 b1 b2 b3 b23 b31 b12 b123) = a0 == b0 && a1 == b1 && a2 == b2 && a3 == b3 && b23 == 0 && b31 == 0 && b12 == 0 && b123 == 0

  (H a0 a23 a31 a12) == (PV b0 b1 b2 b3) = a0 == b0 && a23 == 0 && a31 == 0 && a12 == 0 && b1 == 0 && b2 == 0 && b3 == 0
  (C a0 a123) == (PV b0 b1 b2 b3) = a0 == b0 && a123 == 0 && b1 == 0 && b2 == 0 && b3 == 0
  (BPV a1 a2 a3 a23 a31 a12) == (PV b0 b1 b2 b3) = a1 == b1 && a2 == b2 && a3 == b3 && a23 == 0 && a31 == 0 && a12 == 0 && b0 == 0
  (ODD a1 a2 a3 a123) == (PV b0 b1 b2 b3) = a1 == b1 && a2 == b2 && a3 == b3 && a123 == 0 && b0 == 0
  (TPV a23 a31 a12 a123) == (PV b0 b1 b2 b3) = a23 == 0 && a31 == 0 && a12 == 0 && b0 == 0 && a123 == 0 && b1 == 0 && b2 == 0 && b3 == 0
  (APS a0 a1 a2 a3 a23 a31 a12 a123) == (PV b0 b1 b2 b3) = a0 == b0 && a1 == b1 && a2 == b2 && a3 == b3 && a23 == 0 && a31 == 0 && a12 == 0 && a123 == 0

  (H a0 a23 a31 a12) == (H b0 b23 b31 b12) = a0 == b0 && a23 == b23 && a31 == b31 && a12 == b12

  (H a0 a23 a31 a12) == (C b0 b123) = a0 == b0 && a23 == 0 && a31 == 0 && a12 == 0 && b123 == 0
  (H a0 a23 a31 a12) == (BPV b1 b2 b3 b23 b31 b12) = a0 == 0 && a23 == b23 && a31 == b31 && a12 == b12 && b1 == 0 && b2 == 0 && b3 == 0
  (H a0 a23 a31 a12) == (ODD b1 b2 b3 b123) = a0 == 0 && b1 == 0 && b2 == 0 && b3 == 0 && a23 == 0 && a31 == 0 && a12 == 0 && b123 == 0
  (H a0 a23 a31 a12) == (TPV b23 b31 b12 b123) = a0 == 0 && a23 == b23 && a31 == b31 && a12 == b12 && b123 == 0
  (H a0 a23 a31 a12) == (APS b0 b1 b2 b3 b23 b31 b12 b123) = a0 == b0 && a23 == b23 && a31 == b31 && a12 == b12 && b1 == 0 && b2 == 0 && b3 == 0 && b123 == 0

  (C a0 a123) == (H b0 b23 b31 b12) = a0 == b0 && a123 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (BPV a1 a2 a3 a23 a31 a12) == (H b0 b23 b31 b12) = a1 == 0 && a2 == 0 && a3 == 0 && a23 == b23 && a31 == b31 && a12 == b12 && b0 == 0
  (ODD a1 a2 a3 a123) == (H b0 b23 b31 b12) = a1 == 0 && a2 == 0 && a3 == 0 && a123 == 0 && b23 == 0 && b31 == 0 && b12 == 0 && b0 == 0
  (TPV a23 a31 a12 a123) == (H b0 b23 b31 b12) = a23 == b23 && a31 == b31 && a12 == b12 && b0 == 0 && a123 == 0
  (APS a0 a1 a2 a3 a23 a31 a12 a123) == (H b0 b23 b31 b12) = a0 == b0 && a1 == 0 && a2 == 0 && a3 == 0 && a23 == b23 && a31 == b31 && a12 == b12 && a123 == 0

  (C a0 a123) == (C b0 b123) = a0 == b0 && a123 == b123

  (C a0 a123) == (BPV b1 b2 b3 b23 b31 b12) = a0 == 0 && a123 == 0 && b1 == 0 && b2 == 0 && b3 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (C a0 a123) == (ODD b1 b2 b3 b123) = a0 == 0 && a123 == b123 && b1 == 0 && b2 == 0 && b3 == 0
  (C a0 a123) == (TPV b23 b31 b12 b123) = a0 == 0 && a123 == b123 && b23 == 0 && b31 == 0 && b12 == 0
  (C a0 a123) == (APS b0 b1 b2 b3 b23 b31 b12 b123) = a0 == b0 && a123 == b123 && b1 == 0 && b2 == 0 && b3 == 0 && b23 == 0 && b31 == 0 && b12 == 0

  (BPV a1 a2 a3 a23 a31 a12) == (C b0 b123) = a1 == 0 && a2 == 0 && a3 == 0 && a23 == 0 && a31 == 0 && a12 == 0 && b0 == 0 && b123 == 0
  (ODD a1 a2 a3 a123) == (C b0 b123) = b0 == 0 && a123 == b123 && a1 == 0 && a2 == 0 && a3 == 0
  (TPV a23 a31 a12 a123) == (C b0 b123) = b0 == 0 && a123 == b123 && a23 == 0 && a31 == 0 && a12 == 0
  (APS a0 a1 a2 a3 a23 a31 a12 a123) == (C b0 b123) = a0 == b0 && a123 == b123 && a1 == 0 && a2 == 0 && a3 == 0 && a23 == 0 && a31 == 0 && a12 == 0

  (BPV a1 a2 a3 a23 a31 a12) == (BPV b1 b2 b3 b23 b31 b12) = a1 == b1 && a2 == b2 && a3 == b3 && a23 == b23 && a31 == b31 && a12 == b12

  (BPV a1 a2 a3 a23 a31 a12) == (ODD b1 b2 b3 b123) = a1 == b1 && a2 == b2 && a3 == b3 && b123 == 0 && a23 == 0 && a31 == 0 && a12 == 0
  (BPV a1 a2 a3 a23 a31 a12) == (TPV b23 b31 b12 b123) = a23 == b23 && a31 == b31 && a12 == b12 && b123 == 0 && a1 == 0 && a2 == 0 && a3 == 0
  (BPV a1 a2 a3 a23 a31 a12) == (APS b0 b1 b2 b3 b23 b31 b12 b123) = a1 == b1 && a2 == b2 && a3 == b3 && a23 == b23 && a31 == b31 && a12 == b12
                                                                              && b0 == 0 && b123 == 0

  (ODD a1 a2 a3 a123) == (BPV b1 b2 b3 b23 b31 b12) = a1 == b1 && a2 == b2 && a3 == b3 && a123 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (TPV a23 a31 a12 a123) == (BPV b1 b2 b3 b23 b31 b12) = a23 == b23 && a31 == b31 && a12 == b12 && a123 == 0 && b1 == 0 && b2 == 0 && b3 == 0
  (APS a0 a1 a2 a3 a23 a31 a12 a123) == (BPV b1 b2 b3 b23 b31 b12) = a0 == 0 && a1 == b1 && a2 == b2 && a3 == b3 && a23 == b23 && a31 == b31
                                                                             && a12 == b12 && a123 == 0

  (ODD a1 a2 a3 a123) == (ODD b1 b2 b3 b123) = a1 == b1 && a2 == b2 && a3 == b3 && a123 == b123

  (ODD a1 a2 a3 a123) == (TPV b23 b31 b12 b123) = a123 == b123 && a1 == 0 && a2 == 0 && a3 == 0 && b23 == 0 && b31 == 0 && b12 == 0
  (ODD a1 a2 a3 a123) == (APS b0 b1 b2 b3 b23 b31 b12 b123) = a1 == b1 && a2 == b2 && a3 == b3 && a123 == b123 && b0 == 0 && b23 == 0 && b31 == 0 && b12 == 0

  (TPV a23 a31 a12 a123) == (ODD b1 b2 b3 b123) = a123 == b123 && b1 == 0 && b2 == 0 && b3 == 0 && a23 == 0 && a31 == 0 && a12 == 0
  (APS a0 a1 a2 a3 a23 a31 a12 a123) == (ODD b1 b2 b3 b123) = a1 == b1 && a2 == b2 && a3 == b3 && a123 == b123 && a0 == 0 && a23 == 0 && a31 == 0 && a12 == 0

  (TPV a23 a31 a12 a123) == (TPV b23 b31 b12 b123) = a23 == b23 && a31 == b31 && a12 == b12 && a123 == b123

  (TPV a23 a31 a12 a123) == (APS b0 b1 b2 b3 b23 b31 b12 b123) = a23 == b23 && a31 == b31 && a12 == b12 && a123 == b123
                                                                            && b0 == 0 && b1 == 0 && b2 == 0 && b3 == 0

  (APS a0 a1 a2 a3 a23 a31 a12 a123) == (TPV b23 b31 b12 b123) = a23 == b23 && a31 == b31 && a12 == b12 && a123 == b123
                                                                            && a0 == 0 && a1 == 0 && a2 == 0 && a3 == 0

  (APS a0 a1 a2 a3 a23 a31 a12 a123) == (APS b0 b1 b2 b3 b23 b31 b12 b123) = a0 == b0 && a1 == b1 && a2 == b2 && a3 == b3 && a23 == b23
                                                                                      && a31 == b31 && a12 == b12 && a123 == b123


-- |Cl3 has a total preorder ordering in which all pairs are comparable by two real valued functions.
-- Comparison of two reals is just the typical real compare function.  Comparison of to imaginary numbers
-- is just the typical comparison function.  When reals are compared to anything else it will compare the
-- absolute value of the reals to the magnitude of the other cliffor.  Compare of two complex values
-- compares the polar magnitude of the complex numbers.  Compare of two vectors compares the vector
-- magnitudes.  The Ord instance for the general case is based on the singular values of each cliffor and
-- this Ordering compares the largest singular value 'abs' and then the littlest singular value 'lsv'.
-- Some arbitrary cliffors may return EQ for Ord but not be exactly '==' equivalent, but they are related
-- by a right and left multiplication of two unitary elements.  For instance for the Cliffors A and B,
-- A == B could be False, but compare A B is EQ, because A * V = U * B, where V and U are unitary.  
instance Ord Cl3 where
  compare (R a0) (R b0) = compare a0 b0 -- Real Numbers have a total order within the limitations of Double Precision comparison
  compare (I a123) (I b123) = compare a123 b123 -- Imaginary Numbers have a total order within the limitations of Double Precision comparison
  compare cliffor1 cliffor2 =
     let (R a0) = abs cliffor1
         (R b0) = abs cliffor2
         (R a0') = lsv cliffor1
         (R b0') = lsv cliffor2
     in case compare a0 b0 of
          LT -> LT
          GT -> GT
          EQ -> compare a0' b0'



-- |Cl3 has a "Num" instance.  "Num" is addition, geometric product, negation, 'abs' the largest
-- singular value, and 'signum'.
-- 
instance Num Cl3 where
  -- | Cl3 can be added
  (R a0) + (R b0) = R (a0 + b0)

  (R a0) + (V3 b1 b2 b3) = PV a0 b1 b2 b3
  (R a0) + (BV b23 b31 b12) = H a0 b23 b31 b12
  (R a0) + (I b123) = C a0 b123
  (R a0) + (PV b0 b1 b2 b3) = PV (a0 + b0) b1 b2 b3
  (R a0) + (H b0 b23 b31 b12) = H (a0 + b0) b23 b31 b12
  (R a0) + (C b0 b123) = C (a0 + b0) b123
  (R a0) + (BPV b1 b2 b3 b23 b31 b12) = APS a0 b1 b2 b3 b23 b31 b12 0
  (R a0) + (ODD b1 b2 b3 b123) = APS a0 b1 b2 b3 0 0 0 b123
  (R a0) + (TPV b23 b31 b12 b123) = APS a0 0 0 0 b23 b31 b12 b123
  (R a0) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0 + b0) b1 b2 b3 b23 b31 b12 b123

  (V3 a1 a2 a3) + (R b0) = PV b0 a1 a2 a3
  (BV a23 a31 a12) + (R b0) = H b0 a23 a31 a12
  (I a123) + (R b0) = C b0 a123
  (PV a0 a1 a2 a3) + (R b0) = PV (a0 + b0) a1 a2 a3
  (H a0 a23 a31 a12) + (R b0) = H (a0 + b0) a23 a31 a12
  (C a0 a123) + (R b0) = C (a0 + b0) a123
  (BPV a1 a2 a3 a23 a31 a12) + (R b0) = APS b0 a1 a2 a3 a23 a31 a12 0
  (ODD a1 a2 a3 a123) + (R b0) = APS b0 a1 a2 a3 0 0 0 a123
  (TPV a23 a31 a12 a123) + (R b0) = APS b0 0 0 0 a23 a31 a12 a123
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (R b0) = APS (a0 + b0) a1 a2 a3 a23 a31 a12 a123

  (V3 a1 a2 a3) + (V3 b1 b2 b3) = V3 (a1 + b1) (a2 + b2) (a3 + b3)

  (V3 a1 a2 a3) + (BV b23 b31 b12) = BPV a1 a2 a3 b23 b31 b12
  (V3 a1 a2 a3) + (I b123) = ODD a1 a2 a3 b123
  (V3 a1 a2 a3) + (PV b0 b1 b2 b3) = PV b0 (a1 + b1) (a2 + b2) (a3 + b3)
  (V3 a1 a2 a3) + (H b0 b23 b31 b12) = APS b0 a1 a2 a3 b23 b31 b12 0
  (V3 a1 a2 a3) + (C b0 b123) = APS b0 a1 a2 a3 0 0 0 b123
  (V3 a1 a2 a3) + (BPV b1 b2 b3 b23 b31 b12) = BPV (a1 + b1) (a2 + b2) (a3 + b3) b23 b31 b12
  (V3 a1 a2 a3) + (ODD b1 b2 b3 b123) = ODD (a1 + b1) (a2 + b2) (a3 + b3) b123
  (V3 a1 a2 a3) + (TPV b23 b31 b12 b123) = APS 0 a1 a2 a3 b23 b31 b12 b123
  (V3 a1 a2 a3) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS b0 (a1 + b1) (a2 + b2) (a3 + b3) b23 b31 b12 b123

  (BV a23 a31 a12) + (V3 b1 b2 b3) = BPV b1 b2 b3 a23 a31 a12
  (I a123) + (V3 b1 b2 b3) = ODD b1 b2 b3 a123
  (PV a0 a1 a2 a3) + (V3 b1 b2 b3) = PV a0 (a1 + b1) (a2 + b2) (a3 + b3)
  (H a0 a23 a31 a12) + (V3 b1 b2 b3) = APS a0 b1 b2 b3 a23 a31 a12 0
  (C a0 a123) + (V3 b1 b2 b3) = APS a0 b1 b2 b3 0 0 0 a123
  (BPV a1 a2 a3 a23 a31 a12) + (V3 b1 b2 b3) = BPV (a1 + b1) (a2 + b2) (a3 + b3) a23 a31 a12
  (ODD a1 a2 a3 a123) + (V3 b1 b2 b3) = ODD (a1 + b1) (a2 + b2) (a3 + b3) a123
  (TPV a23 a31 a12 a123) + (V3 b1 b2 b3) = APS 0 b1 b2 b3 a23 a31 a12 a123
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (V3 b1 b2 b3) = APS a0 (a1 + b1) (a2 + b2) (a3 + b3) a23 a31 a12 a123

  (BV a23 a31 a12) + (BV b23 b31 b12) = BV (a23 + b23) (a31 + b31) (a12 + b12)

  (BV a23 a31 a12) + (I b123) = TPV a23 a31 a12 b123
  (BV a23 a31 a12) + (PV b0 b1 b2 b3) = APS b0 b1 b2 b3 a23 a31 a12 0
  (BV a23 a31 a12) + (H b0 b23 b31 b12) = H b0 (a23 + b23) (a31 + b31) (a12 + b12)
  (BV a23 a31 a12) + (C b0 b123) = APS b0 0 0 0 a23 a31 a12 b123
  (BV a23 a31 a12) + (BPV b1 b2 b3 b23 b31 b12) = BPV b1 b2 b3 (a23 + b23) (a31 + b31) (a12 + b12)
  (BV a23 a31 a12) + (ODD b1 b2 b3 b123) = APS 0 b1 b2 b3 a23 a31 a12 b123
  (BV a23 a31 a12) + (TPV b23 b31 b12 b123) = TPV (a23 + b23) (a31 + b31) (a12 + b12) b123
  (BV a23 a31 a12) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS b0 b1 b2 b3 (a23 + b23) (a31 + b31) (a12 + b12) b123

  (I a123) + (BV b23 b31 b12) = TPV b23 b31 b12 a123
  (PV a0 a1 a2 a3) + (BV b23 b31 b12) = APS a0 a1 a2 a3 b23 b31 b12 0
  (H a0 a23 a31 a12) + (BV b23 b31 b12) = H a0 (a23 + b23) (a31 + b31) (a12 + b12)
  (C a0 a123) + (BV b23 b31 b12) = APS a0 0 0 0 b23 b31 b12 a123
  (BPV a1 a2 a3 a23 a31 a12) + (BV b23 b31 b12) = BPV a1 a2 a3 (a23 + b23) (a31 + b31) (a12 + b12)
  (ODD a1 a2 a3 a123) + (BV b23 b31 b12) = APS 0 a1 a2 a3 b23 b31 b12 a123
  (TPV a23 a31 a12 a123) + (BV b23 b31 b12) = TPV (a23 + b23) (a31 + b31) (a12 + b12) a123
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (BV b23 b31 b12) = APS a0 a1 a2 a3 (a23 + b23) (a31 + b31) (a12 + b12) a123

  (I a123) + (I b123) = I (a123 + b123)

  (I a123) + (PV b0 b1 b2 b3) = APS b0 b1 b2 b3 0 0 0 a123
  (I a123) + (H b0 b23 b31 b12) = APS b0 0 0 0 b23 b31 b12 a123
  (I a123) + (C b0 b123) = C b0 (a123 + b123)
  (I a123) + (BPV b1 b2 b3 b23 b31 b12) = APS 0 b1 b2 b3 b23 b31 b12 a123
  (I a123) + (ODD b1 b2 b3 b123) = ODD b1 b2 b3 (a123 + b123)
  (I a123) + (TPV b23 b31 b12 b123) = TPV b23 b31 b12 (a123 + b123)
  (I a123) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS b0 b1 b2 b3 b23 b31 b12 (a123 + b123)

  (PV a0 a1 a2 a3) + (I b123) = APS a0 a1 a2 a3 0 0 0 b123
  (H a0 a23 a31 a12) + (I b123) = APS a0 0 0 0 a23 a31 a12 b123
  (C a0 a123) + (I b123) = C a0 (a123 + b123)
  (BPV a1 a2 a3 a23 a31 a12) + (I b123) = APS 0 a1 a2 a3 a23 a31 a12 b123
  (ODD a1 a2 a3 a123) + (I b123) = ODD a1 a2 a3 (a123 + b123)
  (TPV a23 a31 a12 a123) + (I b123) = TPV a23 a31 a12 (a123 + b123)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (I b123) = APS a0 a1 a2 a3 a23 a31 a12 (a123 + b123)

  (PV a0 a1 a2 a3) + (PV b0 b1 b2 b3) = PV (a0 + b0) (a1 + b1) (a2 + b2) (a3 + b3)

  (PV a0 a1 a2 a3) + (H b0 b23 b31 b12) = APS (a0 + b0) a1 a2 a3 b23 b31 b12 0
  (PV a0 a1 a2 a3) + (C b0 b123) = APS (a0 + b0) a1 a2 a3 0 0 0 b123
  (PV a0 a1 a2 a3) + (BPV b1 b2 b3 b23 b31 b12) = APS a0 (a1 + b1) (a2 + b2) (a3 + b3) b23 b31 b12 0
  (PV a0 a1 a2 a3) + (ODD b1 b2 b3 b123) = APS a0 (a1 + b1) (a2 + b2) (a3 + b3) 0 0 0 b123
  (PV a0 a1 a2 a3) + (TPV b23 b31 b12 b123) = APS a0 a1 a2 a3 b23 b31 b12 b123
  (PV a0 a1 a2 a3) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0 + b0) (a1 + b1) (a2 + b2) (a3 + b3) b23 b31 b12 b123

  (H a0 a23 a31 a12) + (PV b0 b1 b2 b3) = APS (a0 + b0) b1 b2 b3 a23 a31 a12 0
  (C a0 a123) + (PV b0 b1 b2 b3) = APS (a0 + b0) b1 b2 b3 0 0 0 a123
  (BPV a1 a2 a3 a23 a31 a12) + (PV b0 b1 b2 b3) = APS b0 (a1 + b1) (a2 + b2) (a3 + b3) a23 a31 a12 0
  (ODD a1 a2 a3 a123) + (PV b0 b1 b2 b3) = APS b0 (a1 + b1) (a2 + b2) (a3 + b3) 0 0 0 a123
  (TPV a23 a31 a12 a123) + (PV b0 b1 b2 b3) = APS b0 b1 b2 b3 a23 a31 a12 a123
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (PV b0 b1 b2 b3) = APS (a0 + b0) (a1 + b1) (a2 + b2) (a3 + b3) a23 a31 a12 a123

  (H a0 a23 a31 a12) + (H b0 b23 b31 b12) = H (a0 + b0) (a23 + b23) (a31 + b31) (a12 + b12)

  (H a0 a23 a31 a12) + (C b0 b123) = APS (a0 + b0) 0 0 0 a23 a31 a12 b123
  (H a0 a23 a31 a12) + (BPV b1 b2 b3 b23 b31 b12) = APS a0 b1 b2 b3 (a23 + b23) (a31 + b31) (a12 + b12) 0
  (H a0 a23 a31 a12) + (ODD b1 b2 b3 b123) = APS a0 b1 b2 b3 a23 a31 a12 b123
  (H a0 a23 a31 a12) + (TPV b23 b31 b12 b123) = APS a0 0 0 0 (a23 + b23) (a31 + b31) (a12 + b12) b123
  (H a0 a23 a31 a12) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0 + b0) b1 b2 b3 (a23 + b23) (a31 + b31) (a12 + b12) b123

  (C a0 a123) + (H b0 b23 b31 b12) = APS (a0 + b0) 0 0 0 b23 b31 b12 a123
  (BPV a1 a2 a3 a23 a31 a12) + (H b0 b23 b31 b12) = APS b0 a1 a2 a3 (a23 + b23) (a31 + b31) (a12 + b12) 0
  (ODD a1 a2 a3 a123) + (H b0 b23 b31 b12) = APS b0 a1 a2 a3 b23 b31 b12 a123
  (TPV a23 a31 a12 a123) + (H b0 b23 b31 b12) = APS b0 0 0 0 (a23 + b23) (a31 + b31) (a12 + b12) a123
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (H b0 b23 b31 b12) = APS (a0 + b0) a1 a2 a3 (a23 + b23) (a31 + b31) (a12 + b12) a123

  (C a0 a123) + (C b0 b123) = C (a0 + b0) (a123 + b123)

  (C a0 a123) + (BPV b1 b2 b3 b23 b31 b12) = APS a0 b1 b2 b3 b23 b31 b12 a123
  (C a0 a123) + (ODD b1 b2 b3 b123) = APS a0 b1 b2 b3 0 0 0 (a123 + b123)
  (C a0 a123) + (TPV b23 b31 b12 b123) = APS a0 0 0 0 b23 b31 b12 (a123 + b123)
  (C a0 a123) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0 + b0) b1 b2 b3 b23 b31 b12 (a123 + b123)

  (BPV a1 a2 a3 a23 a31 a12) + (C b0 b123) = APS b0 a1 a2 a3 a23 a31 a12 b123
  (ODD a1 a2 a3 a123) + (C b0 b123) = APS b0 a1 a2 a3 0 0 0 (a123 + b123)
  (TPV a23 a31 a12 a123) + (C b0 b123) = APS b0 0 0 0 a23 a31 a12 (a123 + b123)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (C b0 b123) = APS (a0 + b0) a1 a2 a3 a23 a31 a12 (a123 + b123)

  (BPV a1 a2 a3 a23 a31 a12) + (BPV b1 b2 b3 b23 b31 b12) = BPV (a1 + b1) (a2 + b2) (a3 + b3) (a23 + b23) (a31 + b31) (a12 + b12)

  (BPV a1 a2 a3 a23 a31 a12) + (ODD b1 b2 b3 b123) = APS 0 (a1 + b1) (a2 + b2) (a3 + b3) a23 a31 a12 b123
  (BPV a1 a2 a3 a23 a31 a12) + (TPV b23 b31 b12 b123) = APS 0 a1 a2 a3 (a23 + b23) (a31 + b31) (a12 + b12) b123
  (BPV a1 a2 a3 a23 a31 a12) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS b0 (a1 + b1) (a2 + b2) (a3 + b3) (a23 + b23) (a31 + b31) (a12 + b12) b123

  (ODD a1 a2 a3 a123) + (BPV b1 b2 b3 b23 b31 b12) = APS 0 (a1 + b1) (a2 + b2) (a3 + b3) b23 b31 b12 a123
  (TPV a23 a31 a12 a123) + (BPV b1 b2 b3 b23 b31 b12) = APS 0 b1 b2 b3 (a23 + b23) (a31 + b31) (a12 + b12) a123
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (BPV b1 b2 b3 b23 b31 b12) = APS a0 (a1 + b1) (a2 + b2) (a3 + b3) (a23 + b23) (a31 + b31) (a12 + b12) a123

  (ODD a1 a2 a3 a123) + (ODD b1 b2 b3 b123) = ODD (a1 + b1) (a2 + b2) (a3 + b3) (a123 + b123)

  (ODD a1 a2 a3 a123) + (TPV b23 b31 b12 b123) = APS 0 a1 a2 a3 b23 b31 b12 (a123 + b123)
  (ODD a1 a2 a3 a123) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS b0 (a1 + b1) (a2 + b2) (a3 + b3) b23 b31 b12 (a123 + b123)

  (TPV a23 a31 a12 a123) + (ODD b1 b2 b3 b123) = APS 0 b1 b2 b3 a23 a31 a12 (a123 + b123)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (ODD b1 b2 b3 b123) = APS a0 (a1 + b1) (a2 + b2) (a3 + b3) a23 a31 a12 (a123 + b123)

  (TPV a23 a31 a12 a123) + (TPV b23 b31 b12 b123) = TPV (a23 + b23) (a31 + b31) (a12 + b12) (a123 + b123)

  (TPV a23 a31 a12 a123) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS b0 b1 b2 b3 (a23 + b23) (a31 + b31) (a12 + b12) (a123 + b123)

  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (TPV b23 b31 b12 b123) = APS a0 a1 a2 a3 (a23 + b23) (a31 + b31) (a12 + b12) (a123 + b123)

  (APS a0 a1 a2 a3 a23 a31 a12 a123) + (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0 + b0)
                                                                                (a1 + b1) (a2 + b2) (a3 + b3)
                                                                                (a23 + b23) (a31 + b31) (a12 + b12)
                                                                                (a123 + b123)

  -- | Multiplication Instance implementing a Geometric Product
  (R a0) * (R b0) = R (a0*b0)

  (R a0) * (V3 b1 b2 b3) = V3 (a0*b1) (a0*b2) (a0*b3)
  (R a0) * (BV b23 b31 b12) = BV (a0*b23) (a0*b31) (a0*b12)
  (R a0) * (I b123) = I (a0*b123)
  (R a0) * (PV b0 b1 b2 b3) = PV (a0*b0)
                                 (a0*b1) (a0*b2) (a0*b3)
  (R a0) * (H b0 b23 b31 b12) = H (a0*b0)
                                  (a0*b23) (a0*b31) (a0*b12)
  (R a0) * (C b0 b123) = C (a0*b0)
                           (a0*b123)
  (R a0) * (BPV b1 b2 b3 b23 b31 b12) = BPV (a0*b1) (a0*b2) (a0*b3)
                                            (a0*b23) (a0*b31) (a0*b12)
  (R a0) * (ODD b1 b2 b3 b123) = ODD (a0*b1) (a0*b2) (a0*b3)
                                     (a0*b123)
  (R a0) * (TPV b23 b31 b12 b123) = TPV (a0*b23) (a0*b31) (a0*b12)
                                        (a0*b123)
  (R a0) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0*b0)
                                                    (a0*b1) (a0*b2) (a0*b3)
                                                    (a0*b23) (a0*b31) (a0*b12)
                                                    (a0*b123)

  (V3 a1 a2 a3) * (R b0) = V3 (a1*b0) (a2*b0) (a3*b0)
  (BV a23 a31 a12) * (R b0) = BV (a23*b0) (a31*b0) (a12*b0)
  (I a123) * (R b0) = I (a123*b0)
  (PV a0 a1 a2 a3) * (R b0) = PV (a0*b0)
                                 (a1*b0) (a2*b0) (a3*b0)
  (H a0 a23 a31 a12) * (R b0) = H (a0*b0)
                                  (a23*b0) (a31*b0) (a12*b0)
  (C a0 a123) * (R b0) = C (a0*b0)
                           (a123*b0)
  (BPV a1 a2 a3 a23 a31 a12) * (R b0) = BPV (a1*b0) (a2*b0) (a3*b0)
                                            (a23*b0) (a31*b0) (a12*b0)
  (ODD a1 a2 a3 a123) * (R b0) = ODD (a1*b0) (a2*b0) (a3*b0)
                                     (a123*b0)
  (TPV a23 a31 a12 a123) * (R b0) = TPV (a23*b0) (a31*b0) (a12*b0)
                                        (a123*b0)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (R b0) = APS (a0*b0)
                                                    (a1*b0) (a2*b0) (a3*b0)
                                                    (a23*b0) (a31*b0) (a12*b0)
                                                    (a123*b0)

  (V3 a1 a2 a3) * (V3 b1 b2 b3) = H (a1*b1 + a2*b2 + a3*b3)
                                    (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)

  (V3 a1 a2 a3) * (BV b23 b31 b12) = ODD (a3*b31 - a2*b12) (a1*b12 - a3*b23) (a2*b23 - a1*b31)
                                         (a1*b23 + a2*b31 + a3*b12)
  (V3 a1 a2 a3) * (I b123) = BV (a1*b123) (a2*b123) (a3*b123)
  (V3 a1 a2 a3) * (PV b0 b1 b2 b3) = APS (a1*b1 + a2*b2 + a3*b3)
                                         (a1*b0) (a2*b0) (a3*b0)
                                         (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)
                                         0
  (V3 a1 a2 a3) * (H b0 b23 b31 b12) = ODD (a1*b0 - a2*b12 + a3*b31) (a2*b0 + a1*b12 - a3*b23) (a3*b0 - a1*b31 + a2*b23)
                                           (a1*b23 + a2*b31 + a3*b12)
  (V3 a1 a2 a3) * (C b0 b123) = BPV (a1*b0) (a2*b0) (a3*b0)
                                    (a1*b123) (a2*b123) (a3*b123)
  (V3 a1 a2 a3) * (BPV b1 b2 b3 b23 b31 b12) = APS (a1*b1 + a2*b2 + a3*b3)
                                                   (a3*b31 - a2*b12) (a1*b12 - a3*b23) (a2*b23 - a1*b31)
                                                   (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)
                                                   (a1*b23 + a2*b31 + a3*b12)
  (V3 a1 a2 a3) * (ODD b1 b2 b3 b123) = H (a1*b1 + a2*b2 + a3*b3)
                                          (a1*b123 + a2*b3 - a3*b2) (a2*b123 - a1*b3 + a3*b1) (a3*b123 + a1*b2 - a2*b1)
  (V3 a1 a2 a3) * (TPV b23 b31 b12 b123) = APS 0
                                               (a3*b31 - a2*b12) (a1*b12 - a3*b23) (a2*b23 - a1*b31)
                                               (a1*b123) (a2*b123) (a3*b123)
                                               (a1*b23 + a2*b31 + a3*b12)
  (V3 a1 a2 a3) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a1*b1 + a2*b2 + a3*b3)
                                                           (a1*b0 - a2*b12 + a3*b31) (a2*b0 + a1*b12 - a3*b23) (a3*b0 - a1*b31 + a2*b23)
                                                           (a1*b123 + a2*b3 - a3*b2) (a3*b1 - a1*b3 + a2*b123) (a1*b2 - a2*b1 + a3*b123)
                                                           (a1*b23 + a2*b31 + a3*b12)

  (BV a23 a31 a12) * (V3 b1 b2 b3) = ODD (a12*b2  - a31*b3) (a23*b3 - a12*b1) (a31*b1  - a23*b2)
                                         (a23*b1  + a31*b2  + a12*b3)
  (I a123) * (V3 b1 b2 b3) = BV (a123*b1) (a123*b2) (a123*b3)
  (PV a0 a1 a2 a3) * (V3 b1 b2 b3) = APS (a1*b1 + a2*b2 + a3*b3)
                                         (a0*b1) (a0*b2) (a0*b3)
                                         (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)
                                         0
  (H a0 a23 a31 a12) * (V3 b1 b2 b3) = ODD (a0*b1 + a12*b2 - a31*b3) (a0*b2 - a12*b1 + a23*b3) (a0*b3 + a31*b1 - a23*b2)
                                           (a23*b1 + a31*b2 + a12*b3)
  (C a0 a123) * (V3 b1 b2 b3) = BPV (a0*b1) (a0*b2) (a0*b3)
                                    (a123*b1) (a123*b2) (a123*b3)
  (BPV a1 a2 a3 a23 a31 a12) * (V3 b1 b2 b3) = APS (a1*b1 + a2*b2 + a3*b3)
                                                   (a12*b2 - a31*b3) (a23*b3 - a12*b1) (a31*b1 - a23*b2)
                                                   (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)
                                                   (a23*b1 + a31*b2 + a12*b3)
  (ODD a1 a2 a3 a123) * (V3 b1 b2 b3) = H (a1*b1 + a2*b2 + a3*b3)
                                          (a123*b1 + a2*b3 - a3*b2) (a123*b2 - a1*b3 + a3*b1) (a123*b3 + a1*b2 - a2*b1)
  (TPV a23 a31 a12 a123) * (V3 b1 b2 b3) = APS 0
                                               (a12*b2 - a31*b3) (a23*b3 - a12*b1) (a31*b1 - a23*b2)
                                               (a123*b1) (a123*b2) (a123*b3)
                                               (a23*b1 + a31*b2 + a12*b3)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (V3 b1 b2 b3) = APS (a1*b1 + a2*b2 + a3*b3)
                                                           (a0*b1 + a12*b2 - a31*b3) (a0*b2 - a12*b1 + a23*b3) (a0*b3 + a31*b1 - a23*b2)
                                                           (a123*b1 + a2*b3 - a3*b2) (a3*b1 - a1*b3 + a123*b2) (a1*b2 - a2*b1 + a123*b3)
                                                           (a23*b1 + a31*b2 + a12*b3)

  (BV a23 a31 a12) * (BV b23 b31 b12) = H (negate $ a23*b23 + a31*b31 + a12*b12)
                                          (a12*b31 - a31*b12) (a23*b12 - a12*b23) (a31*b23 - a23*b31)

  (BV a23 a31 a12) * (I b123) = V3 (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
  (BV a23 a31 a12) * (PV b0 b1 b2 b3) = APS 0
                                            (a12*b2 - a31*b3) (a23*b3 - a12*b1) (a31*b1 - a23*b2)
                                            (a23*b0) (a31*b0) (a12*b0)
                                            (a23*b1 + a31*b2 + a12*b3)
  (BV a23 a31 a12) * (H b0 b23 b31 b12) = H (negate $ a23*b23 + a31*b31 + a12*b12)
                                            (a23*b0 - a31*b12 + a12*b31) (a31*b0 + a23*b12 - a12*b23) (a12*b0 - a23*b31 + a31*b23)
  (BV a23 a31 a12) * (C b0 b123) = BPV (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                       (a23*b0) (a31*b0) (a12*b0)
  (BV a23 a31 a12) * (BPV b1 b2 b3 b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                      (a12*b2 - a31*b3) (a23*b3 - a12*b1) (a31*b1 - a23*b2)  
                                                      (a12*b31 - a31*b12) (a23*b12 - a12*b23) (a31*b23 - a23*b31)
                                                      (a23*b1 + a31*b2 + a12*b3)
  (BV a23 a31 a12) * (ODD b1 b2 b3 b123) = ODD (a12*b2 - a31*b3 - a23*b123) (a23*b3 - a12*b1 - a31*b123) (a31*b1 - a23*b2 - a12*b123)
                                               (a23*b1 + a31*b2 + a12*b3)
  (BV a23 a31 a12) * (TPV b23 b31 b12 b123) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                  (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                                  (a12*b31 - a31*b12) (a23*b12 - a12*b23) (a31*b23 - a23*b31)
                                                  0
  (BV a23 a31 a12) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                              (a12*b2 - a31*b3 - a23*b123) (a23*b3 - a31*b123 - a12*b1) (a31*b1 - a23*b2 - a12*b123)
                                                              (a23*b0 - a31*b12 + a12*b31) (a31*b0 + a23*b12 - a12*b23) (a12*b0 - a23*b31 + a31*b23)
                                                              (a23*b1 + a31*b2 + a12*b3)

  (I a123) * (BV b23 b31 b12) = V3 (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
  (PV a0 a1 a2 a3) * (BV b23 b31 b12) = APS 0
                                            (a3*b31 - a2*b12) (a1*b12 - a3*b23) (a2*b23 - a1*b31)
                                            (a0*b23) (a0*b31) (a0*b12)
                                            (a1*b23 + a2*b31 + a3*b12)
  (H a0 a23 a31 a12) * (BV b23 b31 b12) = H (negate $ a23*b23 + a31*b31 + a12*b12)
                                            (a0*b23 - a31*b12 + a12*b31) (a0*b31 + a23*b12 - a12*b23) (a0*b12 - a23*b31 + a31*b23)
  (C a0 a123) * (BV b23 b31 b12) = BPV (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                       (a0*b23) (a0*b31) (a0*b12)
  (BPV a1 a2 a3 a23 a31 a12) * (BV b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                      (a3*b31 - a2*b12) (a1*b12 - a3*b23) (a2*b23 - a1*b31)    
                                                      (a12*b31 - a31*b12) (a23*b12 - a12*b23) (a31*b23 - a23*b31)
                                                      (a1*b23 + a2*b31 + a3*b12)
  (ODD a1 a2 a3 a123) * (BV b23 b31 b12) = ODD (negate $ a123*b23 + a2*b12 - a3*b31)
                                               (negate $ a123*b31 - a1*b12 + a3*b23)
                                               (negate $ a123*b12 + a1*b31 - a2*b23)
                                               (a1*b23 + a2*b31 + a3*b12)
  (TPV a23 a31 a12 a123) * (BV b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                  (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                                  (negate $ a31*b12 - a12*b31) (negate $ a12*b23 - a23*b12) (negate $ a23*b31 - a31*b23)
                                                  0
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (BV b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)  
                                                              (a3*b31 - a123*b23 - a2*b12) (a1*b12 - a3*b23 - a123*b31) (a2*b23 - a123*b12 - a1*b31)
                                                              (a0*b23 - a31*b12 + a12*b31) (a0*b31 + a23*b12 - a12*b23) (a0*b12 - a23*b31 + a31*b23)
                                                              (a1*b23 + a2*b31 + a3*b12)

  (I a123) * (I b123) = R (negate $ a123*b123)

  (I a123) * (PV b0 b1 b2 b3) = TPV (a123*b1) (a123*b2) (a123*b3)
                                    (a123*b0)
  (I a123) * (H b0 b23 b31 b12) = ODD (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                      (a123*b0)
  (I a123) * (C b0 b123) = C (negate $ a123*b123)
                             (a123*b0)
  (I a123) * (BPV b1 b2 b3 b23 b31 b12) = BPV (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                              (a123*b1) (a123*b2) (a123*b3)
  (I a123) * (ODD b1 b2 b3 b123) = H (negate $ a123*b123)
                                     (a123*b1) (a123*b2) (a123*b3)
  (I a123) * (TPV b23 b31 b12 b123) = PV (negate $ a123*b123)
                                         (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
  (I a123) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (negate $ a123*b123)
                                                      (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                                      (a123*b1) (a123*b2) (a123*b3)
                                                      (a123*b0)

  (PV a0 a1 a2 a3) * (I b123) = TPV (a1*b123) (a2*b123) (a3*b123)
                                    (a0*b123)
  (H a0 a23 a31 a12) * (I b123) = ODD (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                      (a0*b123)
  (C a0 a123) * (I b123) = C (negate $ a123*b123)
                             (a0*b123)
  (BPV a1 a2 a3 a23 a31 a12) * (I b123) = BPV (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                              (a1*b123) (a2*b123) (a3*b123)
  (ODD a1 a2 a3 a123) * (I b123) = H (negate $ a123*b123)
                                     (a1*b123) (a2*b123) (a3*b123)
  (TPV a23 a31 a12 a123) * (I b123) = PV (negate $ a123*b123)
                                         (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (I b123) = APS (negate $ a123*b123)
                                                      (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                                      (a1*b123) (a2*b123) (a3*b123)
                                                      (a0*b123)


  (PV a0 a1 a2 a3) * (PV b0 b1 b2 b3) = APS (a0*b0 + a1*b1 + a2*b2 + a3*b3)
                                            (a0*b1 + a1*b0) (a0*b2 + a2*b0) (a0*b3 + a3*b0)
                                            (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1)
                                            0

  (PV a0 a1 a2 a3) * (H b0 b23 b31 b12) = APS (a0*b0)
                                              (a1*b0 - a2*b12 + a3*b31) (a2*b0 + a1*b12 - a3*b23) (a3*b0 - a1*b31 + a2*b23)
                                              (a0*b23) (a0*b31) (a0*b12)
                                              (a1*b23 + a2*b31 + a3*b12)
  (PV a0 a1 a2 a3) * (C b0 b123) = APS (a0*b0)
                                       (a1*b0) (a2*b0) (a3*b0)
                                       (a1*b123) (a2*b123) (a3*b123)
                                       (a0*b123)
  (PV a0 a1 a2 a3) * (BPV b1 b2 b3 b23 b31 b12) = APS (a1*b1 + a2*b2 + a3*b3)
                                                      (a0*b1 - a2*b12 + a3*b31) (a0*b2 + a1*b12 - a3*b23) (a0*b3 - a1*b31 + a2*b23)
                                                      (a0*b23 + a2*b3 - a3*b2) (a0*b31 - a1*b3 + a3*b1) (a0*b12 + a1*b2 - a2*b1)
                                                      (a1*b23 + a2*b31 + a3*b12)
  (PV a0 a1 a2 a3) * (ODD b1 b2 b3 b123) = APS (a1*b1 + a2*b2 + a3*b3)
                                               (a0*b1) (a0*b2) (a0*b3)
                                               (a1*b123 + a2*b3 - a3*b2) (a2*b123 - a1*b3 + a3*b1) (a3*b123 + a1*b2 - a2*b1)
                                               (a0*b123)
  (PV a0 a1 a2 a3) * (TPV b23 b31 b12 b123) = APS 0
                                                  (a3*b31 - a2*b12) (a1*b12 - a3*b23) (a2*b23 - a1*b31)
                                                  (a0*b23 + a1*b123) (a0*b31 + a2*b123) (a0*b12 + a3*b123)
                                                  (a0*b123 + a1*b23 + a2*b31 + a3*b12)
  (PV a0 a1 a2 a3) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0*b0 + a1*b1 + a2*b2 + a3*b3)
                                                              (a0*b1 + a1*b0 - a2*b12 + a3*b31)
                                                              (a0*b2 + a2*b0 + a1*b12 - a3*b23)
                                                              (a0*b3 + a3*b0 - a1*b31 + a2*b23)
                                                              (a0*b23 + a1*b123 + a2*b3 - a3*b2)
                                                              (a0*b31 - a1*b3 + a3*b1 + a2*b123)
                                                              (a0*b12 + a1*b2 - a2*b1 + a3*b123)
                                                              (a0*b123 + a1*b23 + a2*b31 + a3*b12)

  (H a0 a23 a31 a12) * (PV b0 b1 b2 b3) = APS (a0*b0)
                                              (a0*b1 + a12*b2 - a31*b3) (a0*b2 - a12*b1 + a23*b3) (a0*b3 + a31*b1 - a23*b2)
                                              (a23*b0) (a31*b0) (a12*b0)
                                              (a23*b1 + a31*b2 + a12*b3)
  (C a0 a123) * (PV b0 b1 b2 b3) = APS (a0*b0)
                                       (a0*b1) (a0*b2) (a0*b3)
                                       (a123*b1) (a123*b2) (a123*b3)
                                       (a123*b0)
  (BPV a1 a2 a3 a23 a31 a12) * (PV b0 b1 b2 b3) = APS (a1*b1 + a2*b2 + a3*b3)
                                                      (a1*b0 + a12*b2 - a31*b3) (a2*b0 - a12*b1 + a23*b3) (a3*b0 + a31*b1 - a23*b2)
                                                      (a23*b0 + a2*b3 - a3*b2) (a31*b0 - a1*b3 + a3*b1) (a12*b0 + a1*b2 - a2*b1)
                                                      (a23*b1 + a31*b2 + a12*b3)
  (ODD a1 a2 a3 a123) * (PV b0 b1 b2 b3) = APS (a1*b1 + a2*b2 + a3*b3)
                                               (a1*b0) (a2*b0) (a3*b0)
                                               (a123*b1 + a2*b3 - a3*b2)
                                               (a123*b2 - a1*b3 + a3*b1)
                                               (a123*b3 + a1*b2 - a2*b1)
                                               (a123*b0)
  (TPV a23 a31 a12 a123) * (PV b0 b1 b2 b3) = APS 0
                                                  (a12*b2 - a31*b3) (a23*b3 - a12*b1) (a31*b1 - a23*b2)
                                                  (a23*b0 + a123*b1) (a31*b0 + a123*b2) (a12*b0 + a123*b3)
                                                  (a123*b0 + a23*b1 + a31*b2 + a12*b3)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (PV b0 b1 b2 b3) = APS (a0*b0 + a1*b1 + a2*b2 + a3*b3)
                                                              (a0*b1 + a1*b0 + a12*b2 - a31*b3)
                                                              (a0*b2 + a2*b0 - a12*b1 + a23*b3)
                                                              (a0*b3 + a3*b0 + a31*b1 - a23*b2)
                                                              (a23*b0 + a123*b1 + a2*b3 - a3*b2)
                                                              (a31*b0 - a1*b3 + a3*b1 + a123*b2)
                                                              (a12*b0 + a1*b2 - a2*b1 + a123*b3)
                                                              (a123*b0 + a23*b1 + a31*b2 + a12*b3)

  (H a0 a23 a31 a12) * (H b0 b23 b31 b12) = H (a0*b0 - a23*b23 - a31*b31 - a12*b12)
                                              (a0*b23 + a23*b0 - a31*b12 + a12*b31)
                                              (a0*b31 + a31*b0 + a23*b12 - a12*b23)
                                              (a0*b12 + a12*b0 - a23*b31 + a31*b23)

  (H a0 a23 a31 a12) * (C b0 b123) = APS (a0*b0)
                                         (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                         (a23*b0) (a31*b0) (a12*b0)
                                         (a0*b123)
  (H a0 a23 a31 a12) * (BPV b1 b2 b3 b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                        (a0*b1 + a12*b2 - a31*b3) (a0*b2 - a12*b1 + a23*b3) (a0*b3 + a31*b1 - a23*b2)
                                                        (a0*b23 - a31*b12 + a12*b31) (a0*b31 + a23*b12 - a12*b23) (a0*b12 - a23*b31 + a31*b23)
                                                        (a23*b1 + a31*b2  + a12*b3)
  (H a0 a23 a31 a12) * (ODD b1 b2 b3 b123) = ODD (a0*b1 + a12*b2 - a31*b3 - a23*b123)
                                                 (a0*b2 - a12*b1 + a23*b3 - a31*b123)
                                                 (a0*b3 + a31*b1 - a23*b2 - a12*b123)
                                                 (a0*b123 + a23*b1 + a31*b2 + a12*b3)
  (H a0 a23 a31 a12) * (TPV b23 b31 b12 b123) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                    (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                                    (a0*b23 - a31*b12 + a12*b31) (a0*b31 + a23*b12 - a12*b23) (a0*b12 - a23*b31 + a31*b23)
                                                    (a0*b123)
  (H a0 a23 a31 a12) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0*b0 - a23*b23 - a31*b31 - a12*b12)
                                                                (a0*b1 + a12*b2 - a31*b3 - a23*b123)
                                                                (a0*b2 - a12*b1 + a23*b3 - a31*b123)
                                                                (a0*b3 + a31*b1 - a23*b2 - a12*b123)
                                                                (a0*b23 + a23*b0 - a31*b12 + a12*b31)
                                                                (a0*b31 + a31*b0 + a23*b12 - a12*b23)
                                                                (a0*b12 + a12*b0 - a23*b31 + a31*b23)
                                                                (a0*b123 + a23*b1 + a31*b2 + a12*b3)

  (C a0 a123) * (H b0 b23 b31 b12) = APS (a0*b0)
                                         (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                         (a0*b23) (a0*b31) (a0*b12)
                                         (a123*b0)
  (BPV a1 a2 a3 a23 a31 a12) * (H b0 b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                        (a1*b0 - a2*b12 + a3*b31) (a2*b0 + a1*b12 - a3*b23) (a3*b0 - a1*b31 + a2*b23)
                                                        (a23*b0 - a31*b12 + a12*b31) (a31*b0 + a23*b12 - a12*b23) (a12*b0 - a23*b31 + a31*b23)
                                                        (a1*b23 + a2*b31 + a3*b12)
  (ODD a1 a2 a3 a123) * (H b0 b23 b31 b12) = ODD (a1*b0 - a2*b12 + a3*b31 - a123*b23)
                                                 (a2*b0 + a1*b12 - a3*b23 - a123*b31)
                                                 (a3*b0 - a1*b31 + a2*b23 - a123*b12)
                                                 (a123*b0 + a1*b23 + a2*b31 + a3*b12)
  (TPV a23 a31 a12 a123) * (H b0 b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                    (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                                    (a23*b0 - a31*b12 + a12*b31) (a31*b0 + a23*b12 - a12*b23) (a12*b0 - a23*b31 + a31*b23)
                                                    (a123*b0)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (H b0 b23 b31 b12) = APS (a0*b0 - a23*b23 - a31*b31 - a12*b12)
                                                                (a1*b0 - a2*b12 + a3*b31 - a123*b23)
                                                                (a2*b0 + a1*b12 - a3*b23 - a123*b31)
                                                                (a3*b0 - a1*b31 + a2*b23 - a123*b12)
                                                                (a0*b23 + a23*b0 - a31*b12 + a12*b31)
                                                                (a0*b31 + a31*b0 + a23*b12 - a12*b23)
                                                                (a0*b12 + a12*b0 - a23*b31 + a31*b23)
                                                                (a123*b0 + a1*b23 + a2*b31 + a3*b12)

  (C a0 a123) * (C b0 b123) = C (a0*b0 - a123*b123)
                                (a0*b123 + a123*b0)

  (C a0 a123) * (BPV b1 b2 b3 b23 b31 b12) = BPV (a0*b1 - a123*b23) (a0*b2 - a123*b31) (a0*b3 - a123*b12)
                                                 (a0*b23 + a123*b1) (a0*b31 + a123*b2) (a0*b12 + a123*b3)
  (C a0 a123) * (ODD b1 b2 b3 b123) = APS (negate $ a123*b123)
                                          (a0*b1) (a0*b2) (a0*b3)
                                          (a123*b1) (a123*b2) (a123*b3)
                                          (a0*b123)
  (C a0 a123) * (TPV b23 b31 b12 b123) = APS (negate $ a123*b123)
                                             (negate $ a123*b23) (negate $ a123*b31) (negate $ a123*b12)
                                             (a0*b23) (a0*b31) (a0*b12)
                                             (a0*b123)
  (C a0 a123) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0*b0 - a123*b123)
                                                         (a0*b1 - a123*b23) (a0*b2 - a123*b31) (a0*b3 - a123*b12)
                                                         (a0*b23 + a123*b1) (a0*b31 + a123*b2) (a0*b12 + a123*b3)
                                                         (a0*b123 + a123*b0)

  (BPV a1 a2 a3 a23 a31 a12) * (C b0 b123) = BPV (a1*b0 - a23*b123) (a2*b0 - a31*b123) (a3*b0 - a12*b123)
                                                 (a23*b0 + a1*b123) (a31*b0 + a2*b123) (a12*b0 + a3*b123)
  (ODD a1 a2 a3 a123) * (C b0 b123) = APS (negate $ a123*b123)
                                          (a1*b0) (a2*b0) (a3*b0)
                                          (a1*b123) (a2*b123) (a3*b123)
                                          (a123*b0)
  (TPV a23 a31 a12 a123) * (C b0 b123) = APS (negate $ a123*b123)
                                             (negate $ a23*b123) (negate $ a31*b123) (negate $ a12*b123)
                                             (a23*b0) (a31*b0) (a12*b0)
                                             (a123*b0)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (C b0 b123) = APS (a0*b0 - a123*b123)
                                                         (a1*b0 - a23*b123) (a2*b0 - a31*b123) (a3*b0 - a12*b123)
                                                         (a23*b0 + a1*b123) (a31*b0 + a2*b123) (a12*b0 + a3*b123)
                                                         (a0*b123 + a123*b0)

  (BPV a1 a2 a3 a23 a31 a12) * (BPV b1 b2 b3 b23 b31 b12) = APS (a1*b1 + a2*b2 + a3*b3 - a23*b23 - a31*b31 - a12*b12)
                                                                (a12*b2 - a2*b12 + a3*b31 - a31*b3)
                                                                (a1*b12 - a12*b1 - a3*b23 + a23*b3)
                                                                (a31*b1 - a1*b31 + a2*b23 - a23*b2)
                                                                (a2*b3 - a3*b2 - a31*b12 + a12*b31)
                                                                (a3*b1 - a1*b3 + a23*b12 - a12*b23)
                                                                (a1*b2 - a2*b1 - a23*b31 + a31*b23)
                                                                (a1*b23 + a23*b1 + a2*b31 + a31*b2 + a3*b12 + a12*b3)

  (BPV a1 a2 a3 a23 a31 a12) * (ODD b1 b2 b3 b123) = APS (a1*b1 + a2*b2 + a3*b3)
                                                         (a12*b2 - a31*b3 - a23*b123) (a23*b3 - a12*b1 - a31*b123) (a31*b1 - a23*b2 - a12*b123)
                                                         (a1*b123 + a2*b3 - a3*b2) (a2*b123 - a1*b3 + a3*b1) (a3*b123 + a1*b2 - a2*b1)
                                                         (a23*b1 + a31*b2 + a12*b3)
  (BPV a1 a2 a3 a23 a31 a12) * (TPV b23 b31 b12 b123) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                            (a3*b31 - a2*b12 - a23*b123) (a1*b12 - a3*b23 - a31*b123) (a2*b23 - a1*b31 - a12*b123)
                                                            (a1*b123 - a31*b12 + a12*b31) (a2*b123 + a23*b12 - a12*b23) (a3*b123 - a23*b31 + a31*b23)
                                                            (a1*b23 + a2*b31 + a3*b12)
  (BPV a1 a2 a3 a23 a31 a12) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a1*b1 + a2*b2 + a3*b3 - a23*b23 - a31*b31 - a12*b12)
                                                                        (a1*b0 - a2*b12 + a12*b2 + a3*b31 - a31*b3 - a23*b123)
                                                                        (a2*b0 + a1*b12 - a12*b1 - a3*b23 + a23*b3 - a31*b123)
                                                                        (a3*b0 - a1*b31 + a31*b1 + a2*b23 - a23*b2 - a12*b123)
                                                                        (a23*b0 + a1*b123 + a2*b3 - a3*b2 - a31*b12 + a12*b31)
                                                                        (a31*b0 - a1*b3 + a3*b1 + a2*b123 + a23*b12 - a12*b23)
                                                                        (a12*b0 + a1*b2 - a2*b1 + a3*b123 - a23*b31 + a31*b23)
                                                                        (a1*b23 + a23*b1 + a2*b31 + a31*b2 + a3*b12 + a12*b3)

  (ODD a1 a2 a3 a123) * (BPV b1 b2 b3 b23 b31 b12) = APS (a1*b1 + a2*b2 + a3*b3)
                                                         (a3*b31 - a2*b12 - a123*b23) (a1*b12 - a3*b23 - a123*b31) (a2*b23 - a1*b31 - a123*b12)
                                                         (a123*b1 + a2*b3 - a3*b2) (a123*b2 - a1*b3 + a3*b1) (a123*b3 + a1*b2 - a2*b1)
                                                         (a1*b23 + a2*b31 + a3*b12)
  (TPV a23 a31 a12 a123) * (BPV b1 b2 b3 b23 b31 b12) = APS (negate $ a23*b23 + a31*b31 + a12*b12)
                                                            (a12*b2 - a31*b3 - a123*b23) (a23*b3 - a12*b1 - a123*b31) (a31*b1 - a23*b2 - a123*b12)
                                                            (a123*b1 - a31*b12 + a12*b31) (a123*b2 + a23*b12 - a12*b23) (a123*b3 - a23*b31 + a31*b23)
                                                            (a23*b1 + a31*b2 + a12*b3)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (BPV b1 b2 b3 b23 b31 b12) = APS (a1*b1 + a2*b2 + a3*b3 - a23*b23 - a31*b31 - a12*b12)
                                                                        (a0*b1 - a2*b12 + a12*b2 + a3*b31 - a31*b3 - a123*b23)
                                                                        (a0*b2 + a1*b12 - a12*b1 - a3*b23 + a23*b3 - a123*b31)
                                                                        (a0*b3 - a1*b31 + a31*b1 + a2*b23 - a23*b2 - a123*b12)
                                                                        (a0*b23 + a123*b1 + a2*b3 - a3*b2 - a31*b12 + a12*b31)
                                                                        (a0*b31 - a1*b3 + a3*b1 + a123*b2 + a23*b12 - a12*b23)
                                                                        (a0*b12 + a1*b2 - a2*b1 + a123*b3 - a23*b31 + a31*b23)
                                                                        (a1*b23 + a23*b1 + a2*b31 + a31*b2 + a3*b12 + a12*b3)

  (ODD a1 a2 a3 a123) * (ODD b1 b2 b3 b123) = H (a1*b1 + a2*b2 + a3*b3 - a123*b123)
                                                (a1*b123 + a123*b1 + a2*b3 - a3*b2)
                                                (a2*b123 + a123*b2 - a1*b3 + a3*b1)
                                                (a3*b123 + a123*b3 + a1*b2 - a2*b1)

  (ODD a1 a2 a3 a123) * (TPV b23 b31 b12 b123) = APS (negate $ a123*b123)
                                                     (a3*b31 - a2*b12 - a123*b23) (a1*b12 - a3*b23 - a123*b31) (a2*b23 - a1*b31 - a123*b12)
                                                     (a1*b123) (a2*b123) (a3*b123)
                                                     (a1*b23 + a2*b31 + a3*b12)
  (ODD a1 a2 a3 a123) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a1*b1 + a2*b2 + a3*b3 - a123*b123)
                                                                 (a1*b0 - a2*b12 + a3*b31 - a123*b23)
                                                                 (a2*b0 + a1*b12 - a3*b23 - a123*b31)
                                                                 (a3*b0 - a1*b31 + a2*b23 - a123*b12)
                                                                 (a1*b123 + a123*b1 + a2*b3 - a3*b2)
                                                                 (a2*b123 + a123*b2 - a1*b3 + a3*b1)
                                                                 (a3*b123 + a123*b3 + a1*b2 - a2*b1)
                                                                 (a123*b0 + a1*b23 + a2*b31 + a3*b12)

  (TPV a23 a31 a12 a123) * (ODD b1 b2 b3 b123) = APS (negate $ a123*b123)
                                                     (a12*b2 - a31*b3 - a23*b123) (a23*b3 - a12*b1 - a31*b123) (a31*b1 - a23*b2 - a12*b123)
                                                     (a123*b1) (a123*b2) (a123*b3)
                                                     (a23*b1 + a31*b2 + a12*b3)
  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (ODD b1 b2 b3 b123) = APS (a1*b1 + a2*b2 + a3*b3 - a123*b123)
                                                                 (a0*b1 + a12*b2 - a31*b3 - a23*b123)
                                                                 (a0*b2 - a12*b1 + a23*b3 - a31*b123)
                                                                 (a0*b3 + a31*b1 - a23*b2 - a12*b123)
                                                                 (a1*b123 + a123*b1 + a2*b3 - a3*b2)
                                                                 (a2*b123 + a123*b2 - a1*b3 + a3*b1)
                                                                 (a3*b123 + a123*b3 + a1*b2 - a2*b1)
                                                                 (a0*b123 + a23*b1 + a31*b2 + a12*b3)

  (TPV a23 a31 a12 a123) * (TPV b23 b31 b12 b123) = APS (negate $ a23*b23 + a31*b31 + a12*b12 + a123*b123)
                                                        (negate $ a23*b123 + a123*b23) (negate $ a31*b123 + a123*b31) (negate $ a12*b123 + a123*b12)
                                                        (a12*b31 - a31*b12) (a23*b12 - a12*b23) (a31*b23 - a23*b31)
                                                        0

  (TPV a23 a31 a12 a123) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (negate $ a23*b23 + a31*b31 + a12*b12 + a123*b123)
                                                                    (a12*b2 - a31*b3 - a23*b123 - a123*b23)
                                                                    (a23*b3 - a12*b1 - a31*b123 - a123*b31)
                                                                    (a31*b1 - a23*b2 - a12*b123 - a123*b12)
                                                                    (a23*b0 + a123*b1 - a31*b12 + a12*b31)
                                                                    (a31*b0 + a123*b2 + a23*b12 - a12*b23)
                                                                    (a12*b0 + a123*b3 - a23*b31 + a31*b23)
                                                                    (a123*b0 + a23*b1 + a31*b2 + a12*b3)

  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (TPV b23 b31 b12 b123) = APS (negate $ a23*b23 + a31*b31 + a12*b12 + a123*b123)
                                                                    (a3*b31 - a2*b12 - a23*b123 - a123*b23)
                                                                    (a1*b12 - a3*b23 - a31*b123 - a123*b31)
                                                                    (a2*b23 - a1*b31 - a12*b123 - a123*b12)
                                                                    (a0*b23 + a1*b123 - a31*b12 + a12*b31)
                                                                    (a0*b31 + a2*b123 + a23*b12 - a12*b23)
                                                                    (a0*b12 + a3*b123 - a23*b31 + a31*b23)
                                                                    (a0*b123 + a1*b23 + a2*b31 + a3*b12)

  (APS a0 a1 a2 a3 a23 a31 a12 a123) * (APS b0 b1 b2 b3 b23 b31 b12 b123) = APS (a0*b0 + a1*b1 + a2*b2 + a3*b3 - a23*b23 - a31*b31 - a12*b12 - a123*b123)
                                                                                (a0*b1 + a1*b0 - a2*b12 + a12*b2 + a3*b31 - a31*b3 - a23*b123 - a123*b23)
                                                                                (a0*b2 + a2*b0 + a1*b12 - a12*b1 - a3*b23 + a23*b3 - a31*b123 - a123*b31)
                                                                                (a0*b3 + a3*b0 - a1*b31 + a31*b1 + a2*b23 - a23*b2 - a12*b123 - a123*b12)
                                                                                (a0*b23 + a23*b0 + a1*b123 + a123*b1 + a2*b3 - a3*b2 - a31*b12 + a12*b31)
                                                                                (a0*b31 + a31*b0 - a1*b3 + a3*b1 + a2*b123 + a123*b2 + a23*b12 - a12*b23)
                                                                                (a0*b12 + a12*b0 + a1*b2 - a2*b1 + a3*b123 + a123*b3 - a23*b31 + a31*b23)
                                                                                (a0*b123 + a123*b0 + a1*b23 + a23*b1 + a2*b31 + a31*b2 + a3*b12 + a12*b3)




  -- |'abs' is the spectral norm aka the spectral radius
  -- it is the largest singular value. This function may need to be fiddled with
  -- to make the math a bit safer wrt overflows.  This makes use of the largest
  -- singular value, if the littlest singular value is zero then the element is not
  -- invertable, we can see here that R, C, V3, BV, and H are all invertable, and
  -- by implication R, C, and H are division algebras.
  abs cl3 = fst $ abssignum cl3


  -- |'signum' satisfies the Law "abs x * signum x == x"
  -- kind of cool: signum of a vector is it's unit vector.
  signum cl3 = snd $ abssignum cl3


  -- |'fromInteger'
  fromInteger int = R (fromInteger int)


  -- |'negate' simply distributes into the grade components
  negate (R a0) = R (negate a0)
  negate (V3 a1 a2 a3) = V3 (negate a1) (negate a2) (negate a3)
  negate (BV a23 a31 a12) = BV (negate a23) (negate a31) (negate a12)
  negate (I a123) = I (negate a123)
  negate (PV a0 a1 a2 a3) =  PV (negate a0)
                                (negate a1) (negate a2) (negate a3)
  negate (H a0 a23 a31 a12) = H (negate a0)
                                (negate a23) (negate a31) (negate a12)
  negate (C a0 a123) = C (negate a0)
                         (negate a123)
  negate (BPV a1 a2 a3 a23 a31 a12) = BPV (negate a1) (negate a2) (negate a3)
                                          (negate a23) (negate a31) (negate a12)
  negate (ODD a1 a2 a3 a123) = ODD (negate a1) (negate a2) (negate a3)
                                   (negate a123)
  negate (TPV a23 a31 a12 a123) = TPV (negate a23) (negate a31) (negate a12)
                                      (negate a123)
  negate (APS a0 a1 a2 a3 a23 a31 a12 a123) = APS (negate a0)
                                                  (negate a1) (negate a2) (negate a3)
                                                  (negate a23) (negate a31) (negate a12)
                                                  (negate a123)

-- | 'reimMag' small helper function to calculate magnitude for PV and TPV
reimMag :: Double -> Double -> Double -> Double -> Double
reimMag v0 v1 v2 v3 =
  let sumsqs = v1^2 + v2^2 + v3^2
      x = abs v0 * sqrt sumsqs
  in sqrt (v0^2 + sumsqs + 2*x)

-- |Cl(3,0) has a Fractional instance
instance Fractional Cl3 where
  -- |Some of the sub algebras are division algebras but APS is not a division algebra
  recip (R a0) = R (recip a0)   -- R is a division algebra
  recip cliff = 
    let (R mag) = abs cliff
        recipsqmag = recip mag^2
        negrecipsqmag = negate recipsqmag
        recipmag2 = recip.toR $ cliff * bar cliff
        go_recip (V3 a1 a2 a3) = V3 (recipsqmag * a1) (recipsqmag * a2) (recipsqmag * a3)
        go_recip (BV a23 a31 a12) = BV (negrecipsqmag * a23) (negrecipsqmag * a31) (negrecipsqmag * a12)
        go_recip (I a123) = I (negrecipsqmag * a123)
        go_recip (H a0 a23 a31 a12) = H (recipsqmag * a0) (negrecipsqmag * a23) (negrecipsqmag * a31) (negrecipsqmag * a12)  -- H is a division algebra
        go_recip (C a0 a123) = C (recipsqmag * a0) (negrecipsqmag * a123)   -- C is a division algebra
        go_recip (ODD a1 a2 a3 a123) = ODD (recipsqmag * a1) (recipsqmag * a2) (recipsqmag * a3) (negrecipsqmag * a123)
        go_recip pv@PV{} = recipmag2 * bar pv
        go_recip tpv@TPV{} = recipmag2 * bar tpv
        go_recip cliffor = reduce $ spectraldcmp recip recip' cliffor
    in go_recip cliff


  -- |'fromRational'
  fromRational rat = R (fromRational rat)


-- |Cl(3,0) has a "Floating" instance.
instance Floating Cl3 where
  pi = R pi

  --
  exp (R a0) = R (exp a0)
  exp (I a123) = C (cos a123) (sin a123)
  exp (C a0 a123) =
    let expa0 = exp a0
    in C (expa0 * cos a123) (expa0 * sin a123)
  exp cliffor = spectraldcmp exp exp' cliffor



  --
  log (R a0)
    | a0 >= 0 = R (log a0)
    | a0 == (-1) = I pi
    | otherwise = C (log.negate $ a0) pi
  log (I a123)
    | a123 == 1 = I (pi/2)
    | a123 == (-1) = I (-pi/2)
    | otherwise = C (log.abs $ a123) (signum a123 * (pi/2))
  log (C a0 a123) = C (log (a0^2 + a123^2) / 2) (atan2 a123 a0)
  log cliffor = spectraldcmp log log' cliffor


  --
  sqrt (R a0)
    | a0 >= 0 = R (sqrt a0)
    | otherwise = I (sqrt.negate $ a0)
  sqrt (I a123)
    | a123 == 0 = R 0
    | otherwise =
        let sqrtr = sqrt.abs $ a123
            phiby2 = signum a123 * (pi/4) -- evaluated: atan2 a123 0 / 2
        in C (sqrtr * cos phiby2) (sqrtr * sin phiby2)
  sqrt (C a0 a123) =
    let sqrtr = sqrt.sqrt $ a0^2 + a123^2
        phiby2 = atan2 a123 a0 / 2
    in C (sqrtr * cos phiby2) (sqrtr * sin phiby2)
  sqrt cliffor = spectraldcmp sqrt sqrt' cliffor

  --
  sin (R a0) = R (sin a0)
  sin (I a123)
    | a123 == 0 = R 0
    | otherwise = I (sinh a123)
  sin (C a0 a123) = C (sin a0 * cosh a123) (cos a0 * sinh a123)
  sin cliffor = spectraldcmp sin sin' cliffor

  --
  cos (R a0) = R (cos a0)
  cos (I a123) = R (cosh a123)
  cos (C a0 a123) = C (cos a0 * cosh a123) (negate $ sin a0 * sinh a123)
  cos cliffor = spectraldcmp cos cos' cliffor

  --
  tan (R a0) = R (tan a0)
  tan (I a123)
    | a123 == 0 = R 0
    | otherwise = I (tanh a123)
  tan (reduce -> C a0 a123) =
    let
      m = x2^2 + y2^2
      x1 = sinx*coshy
      y1 = cosx*sinhy
      x2 = cosx*coshy
      y2 = negate $ sinx*sinhy
      sinx  = sin a0
      cosx  = cos a0
      sinhy = sinh a123
      coshy = cosh a123
    in C ((x1*x2 + y1*y2)/m) ((x2*y1 - x1*y2)/m)
  tan cliffor = spectraldcmp tan tan' cliffor



  --
  asin (R a0)
      -- asin (R a0) = I (-1) * log (I 1 * R a0 + sqrt (1 - (R a0)^2))
      -- I (-1) * log (I a0 + sqrt (R 1 - (R a0)^2))
      -- I (-1) * log (I a0 + sqrt (R (1 - a0^2)))
      -- I (-1) * log (I a0 + (I (sqrt.negate $ 1 - a0^2)))
      -- I (-1) * log (I a0 + (sqrt.negate $ 1 - a0^2))
      -- Def ==> log (I a123) = C (log.abs $ a123) (signum a123 * (pi/2))
      -- I (-1) * C (log.abs $ (a0 + (sqrt.negate $ 1 - a0^2))) (signum (a0 + (sqrt.negate $ 1 - a0^2)) * (pi/2))
      -- C (signum (a0 + (sqrt.negate $ 1 - a0^2)) * (pi/2)) (negate.log.abs $ (a0 + (sqrt.negate $ 1 - a0^2)))
    | a0 > 1 = C (pi/2) (negate.log $ (a0 + sqrt (a0^2 - 1)))
      -- I (-1) * log (I a0 + R (sqrt $ 1 - a0^2))
      -- I (-1) * log (C (sqrt $ 1 - a0^2) a0)
      -- Def ==> log (C a0 a123) = C (log.sqrt $ a0^2 + a123^2) (atan2 a123 a0)
      -- I (-1) * C (log.sqrt $ (sqrt $ 1 - a0^2)^2 + a0^2) (atan2 a0 (sqrt $ 1 - a0^2))
      -- C (atan2 a0 (sqrt $ 1 - a0^2)) (negate.log.sqrt $ (sqrt $ 1 - a0^2)^2 + a0^2)
      -- C (atan(a0/(sqrt $ 1 - a0^2))) (negate.log.sqrt $ 1)
      -- Apply sqrt 1 == 1, Apply log 1 == 0, reduce
      -- R (atan(a0/(sqrt $ 1 - a0^2)))
      -- Identity: tan(asin x) = x / (sqrt (1 - x^2))
      -- R (asin a0)
    | a0 >= (-1) = R (asin a0)
      -- I (-1) * log (I a0 + sqrt (R (1 - a0^2)))
      -- I (-1) * log (I (a0 + (sqrt.negate $ 1 - a0^2)))
      -- Def ==> log (I a123) = C (log.abs $ a123) (signum a123 * (pi/2))
      -- I (-1) * C (log.abs $ (a0 + (sqrt.negate $ 1 - a0^2))) (signum (a0 + (sqrt.negate $ 1 - a0^2)) * (pi/2))
      -- C (signum (a0 + (sqrt.negate $ 1 - a0^2)) * (pi/2)) (negate.log.abs $ (a0 + (sqrt.negate $ 1 - a0^2)))
      -- For the negative branch signum is -1
      -- C (-pi/2) (negate.log.abs $ (a0 + (sqrt $ a0^2 - 1)))
    | otherwise = C (-pi/2) (negate.log.abs $ (a0 + sqrt (a0^2 - 1)))
      --
      -- For I:
      -- I (-1) * log (I (1) * I a123 + sqrt (R 1 - (I a123)^2))
      -- I (-1) * log (R (-a123) + sqrt (R 1 - (I a123)^2))
      -- I (-1) * log (R (-a123) + sqrt (R 1 - R (-a123^2)))
      -- I (-1) * log (R (-a123) + sqrt (R (1 + a123^2)))
      -- I (-1) * log (R (-a123) + R (sqrt $ 1 + a123^2))
      -- I (-1) * log (R ((sqrt $ 1 + a123^2) - a123))
      -- ((sqrt $ 1 + a123^2) - a123)) is always positive
      -- Def ==> log (R a0) | a0 >= 0 = R (log a0)
      -- I (-1) * (R (log $ (sqrt $ 1 + a123^2) - a123))
      -- I (negate.log $ (sqrt $ 1 + a123^2) - a123)
      -- I (negate.log $ (sqrt $ 1 + a123^2) - a123)
      -- because ((sqrt $ 1 + a123^2) - a123)) is always positive: negate.log == log.recip
      -- I (log.recip $ (sqrt $ 1 + a123^2) - a123)
      -- recip $ (sqrt $ 1 + a123^2) - a123) == (sqrt $ 1 + a123^2) + a123)
      -- I (log $ (sqrt $ 1 + a123^2) + a123)
      -- I (asinh a123)
  asin (I a123)
    | a123 == 0 = R 0
    | otherwise = I (asinh a123)
    --
  asin (C a0 a123) =
      -- For C:
      -- I (-1) * log (I 1 * C a0 a123 + sqrt (R 1 - (C a0 a123)^2))
      -- I (-1) * log (C (-a123) a0 + sqrt (R 1 - (C a0 a123)^2))
      -- I (-1) * log (C (-a123) a0 + sqrt (C (1 - a0^2 + a123^2) (-2*a0*a123)))
      -- Def ==> sqrt (C a0 a123) = C ((sqrt.sqrt $ a0^2 + a123^2) * cos (atan2 a123 a0 / 2)) ((sqrt.sqrt $ a0^2 + a123^2) * sin (atan2 a123 a0 / 2))
      -- I (-1) * log (C (-a123) a0 + C ((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * cos (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) ((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * sin (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)))
      -- I (-1) * log (C (((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * cos (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) - a123) (((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * sin (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) + a0))
      -- Def ==> log (C a0 a123) = C (log.sqrt $ a0^2 + a123^2) (atan2 a123 a0)
      -- C (atan2 (((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * sin (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) + a0)
      --          (((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * cos (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) - a123))
      --   (negate.log.sqrt $ (((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * cos (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) - a123)^2 +
      --                      (((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * sin (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) + a0)^2)
      -- Collect like terms:
    let theta = atan2 (-2*a0*a123) (1 - a0^2 + a123^2)
        rho = sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2
        b0 = rho * cos (theta/2) - a123
        b123 = rho * sin (theta/2) + a0
    in C (atan2 b123 b0) (log (b0^2 + b123^2) / (-2))
    --
  asin cliffor = spectraldcmp asin asin' cliffor

  --
  acos (R a0)
      -- acos x == (pi/2) - asin x so just subistute
      -- For R a0 > 1:
      -- R (pi/2) - C (pi/2) (negate.log $ (a0 + (sqrt $ a0^2 - 1)))
      -- C 0 (negate.negate.log $ (a0 + (sqrt $ a0^2 - 1)))
      -- I (log $ (a0 + (sqrt $ a0^2 - 1)))
    | a0 > 1 = I (log (a0 + sqrt (a0^2 - 1)))
      -- For R a0 > (-1)
      -- R (pi/2) - R (asin a0) == R (acos a0)
    | a0 >= (-1) = R (acos a0)
      -- For R otherwise:
      -- R (pi/2) - C (-pi/2) (negate.log.abs $ (a0 + (sqrt $ a0^2 - 1)))
      -- C pi (negate.negate.log.abs $ (a0 + (sqrt $ a0^2 - 1)))
      -- C pi (log.abs $ (a0 + (sqrt $ a0^2 - 1)))
    | otherwise = C pi (log.abs $ (a0 + sqrt (a0^2 - 1)))
      --
      -- For I:
      -- asin (I a123)  = I (asinh a123) -- so,
      -- acos x == R (pi/2) - I (asinh a123)
      -- C (pi/2) (negate $ asinh a123)
  acos (I a123)
    | a123 == 0 = R (pi/2)
    | otherwise = C (pi/2) (negate $ asinh a123)
  --
  acos (C a0 a123) =
      -- For C:
      -- asin (C a0 a123) = C (atan2 (((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * sin (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) + a0) (((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * cos (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) - a123)) (negate.log.sqrt $ (((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * cos (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) - a123)^2 + (((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * sin (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) + a0)^2)
      -- acos x == (pi/2) - asin x so just subistute
      -- R (pi/2) - C (atan2 (((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * sin (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) + a0) (((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * cos (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) - a123)) (negate.log.sqrt $ (((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * cos (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) - a123)^2 + (((sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2) * sin (atan2 (-2*a0*a123) (1 - a0^2 + a123^2) / 2)) + a0)^2)
    let theta = atan2 (-2*a0*a123) (1 - a0^2 + a123^2)
        rho = sqrt.sqrt $ (1 - a0^2 + a123^2)^2 + (-2*a0*a123)^2
        b0 = rho * cos (theta/2) - a123
        b123 = rho * sin (theta/2) + a0
    in C ((pi/2) - atan2 b123 b0) (log (b0^2 + b123^2) / 2)
    --
  acos cliffor = spectraldcmp acos acos' cliffor

  --
  atan (R a0) = R (atan a0)
  --
  atan (I a123)
      -- I (0.5) * (log (R 1 - (I 1 * I a123)) - log (R 1 + (I 1 * I a123)))
      -- I (0.5) * (log (R 1 - (R (-a123))) - log (R 1 + (R (-a123))))
      -- I (0.5) * ((log (R (1 + a123))) - log (R (1 - a123)))
      -- Def ==> C (log.negate $ a0) pi for negative a123
      -- I (0.5) * ((log (R (1 + a123))) - (C (log.negate $ (1 - a123)) pi))
      -- Def ==> R (log a0) for positive a123
      -- I (0.5) * ((R (log (1 + a123))) - (C (log.negate $ (1 - a123)) pi))
      -- I (0.5) * (C (log (1 + a123) - (log.negate $ (1 - a123))) (-pi))
      -- C (pi/2) ((log (1 + a123) - (log.negate $ (1 - a123)))/2)
    | a123 > 1 = C (pi/2) (0.5*(log (1 + a123) - log (a123 - 1)))
      -- I (0.5) * (log (R 1 - (I 1 * I a123)) - log (R 1 + (I 1 * I a123)))
      -- I (0.5) * (log (R 1 - (R (-a123))) - log (R 1 + (R (-a123))))
      -- I (0.5) * ((log (R (1 + a123))) - log (R (1 - a123)))
      -- I (0.5) * ((R (log (1 + a123))) - R (log (1 - a123)))
      -- I (0.5) * (R ((log (1 + a123)) - (log (1 - a123))))
      -- I (((log (1 + a123)) - (log (1 - a123)))/2)
      -- I (atanh a123)
    | a123 == 0 = R 0
    | a123 >= (-1) = I (atanh a123)
      -- I (0.5) * (log (R 1 - (I 1 * I a123)) - log (R 1 + (I 1 * I a123)))
      -- I (0.5) * (log (R 1 - (R (-a123))) - log (R 1 + (R (-a123))))
      -- I (0.5) * ((log (R (1 + a123))) - R (log (1 - a123)))
      -- C (-pi/2) (((log.negate $ (1 + a123)) - (log (1 - a123)))/2)
    | otherwise = C (-pi/2) (((log.negate $ (1 + a123)) - log (1 - a123))/2)
      --
      -- I (0.5) * (log (R 1 - (I 1 * C a0 a123)) - log (R 1 + (I 1 * C a0 a123)))
      -- I (0.5) * (log (C (1 + a123) (-a0)) - log (C (1 - a123) a0))
      -- Def ==> log (C a0 a123) = C (log.sqrt $ a0^2 + a123^2) (atan2 a123 a0)
      -- I (0.5) * ((C (log.sqrt $ (1 + a123)^2 + (-a0)^2) (atan2 (-a0) (1 + a123))) - (C (log.sqrt $ (1 - a123)^2 + a0^2) (atan2 a0 (1 - a123))))
      -- I (0.5) * C ((log.sqrt $ (1 + a123)^2 + (-a0)^2) - (log.sqrt $ (1 - a123)^2 + a0^2)) ((atan2 (-a0) (1 + a123)) - (atan2 a0 (1 - a123)))
      -- I (0.5) * C (0.5*((log $ (1 + a123)^2 + a0^2) - (log $ (1 - a123)^2 + a0^2))) ((atan2 (-a0) (1 + a123)) - (atan2 a0 (1 - a123)))
      -- C (((atan2 a0 (1 - a123)) + (atan2 a0 (1 + a123)))/2) (((log $ (1 + a123)^2 + a0^2) - (log $ (1 - a123)^2 + a0^2))/4)
  atan (C a0 a123) = C ((atan2 a0 (1 - a123) + atan2 a0 (1 + a123))/2)
                       ((log ((1 + a123)^2 + a0^2) - log ((1 - a123)^2 + a0^2))/4)
    --
  atan cliffor = spectraldcmp atan atan' cliffor

  --
  sinh (R a0) = R (sinh a0)
  sinh (I a123) = I (sin a123)
  sinh (C a0 a123) = C (cos a123 * sinh a0) (sin a123 * cosh a0)
  sinh cliffor = spectraldcmp sinh sinh' cliffor

  --
  cosh (R a0) = R (cosh a0)
  cosh (I a123) = R (cos a123)
  cosh (C a0 a123) = C (cos a123 * cosh a0) (sin a123 * sinh a0)
  cosh cliffor = spectraldcmp cosh cosh' cliffor

  --
  tanh (R a0) = R (tanh a0)
  tanh (I a123) = I (tan a123)
  tanh (reduce -> C a0 a123) =
    let
      m = x2^2 + y2^2
      x1 = cosy*sinhx
      y1 = siny*coshx
      x2 = cosy*coshx
      y2 = siny*sinhx
      siny  = sin a123
      cosy  = cos a123
      sinhx = sinh a0
      coshx = cosh a0
    in C ((x1*x2 + y1*y2)/m) ((x2*y1 - x1*y2)/m)
  tanh cliffor = spectraldcmp tanh tanh' cliffor

  --
  asinh (R a0) = R (asinh a0)
  --
  asinh (I a123)
      -- log (I a123 + sqrt (R (1 - a123^2)))
      -- 3 branches where between -1 and 1 it is just asin
      -- For a123 > 1:
      -- log (I a123 + I (sqrt.negate $ (1 - a123^2)))
      -- log (I (a123 + (sqrt (a123^2 - 1))))
      -- Def ==> log (I a123) = C (log.abs $ a123) (signum a123 * (pi/2))
      -- C (log.abs $ (a123 + (sqrt (a123^2 - 1)))) (signum (a123 + (sqrt (a123^2 - 1))) * (pi/2))
      -- a123 is positive so signum evaluates to 1
      -- C (log.abs $ (a123 + (sqrt (a123^2 - 1)))) (pi/2)
    | a123 > 1 = C (log.abs $ (a123 + sqrt (a123^2 - 1))) (pi/2)
      -- log (I a123 + sqrt (R (1 - a123^2)))
      -- log (I a123 + R (sqrt (1 - a123^2)))
      -- log (C (sqrt (1 - a123^2)) a123)
      -- Def ==> log (C a0 a123) = C (log.sqrt $ a0^2 + a123^2) (atan2 a123 a0)
      -- C (log.sqrt $ (sqrt (1 - a123^2))^2 + a123^2) (atan2 a123 (sqrt (1 - a123^2)))
      -- (sqrt (1 - a123^2))^2 + a123^2 == 1
      -- sqrt 1 == 1
      -- log 1 == 0
      -- I (atan2 a123 (sqrt (1 - a123^2)))
      -- I (atan (a123 / (sqrt (1 - a123^2))))
      -- Identity: tan(asin x) = x / (sqrt (1 - x^2))
      -- asin a123 = atan (a123 / (sqrt (1 - a123^2)))
    | a123 == 0 = R 0
    | a123 >= (-1) = I (asin a123)
      -- log (I a123 + sqrt (R (1 - a123^2)))
      -- For a123 < (-1):
      -- log (I a123 + I (sqrt.negate $ (1 - a123^2)))
      -- log (I (a123 + (sqrt (a123^2 - 1))))
      -- Def ==> log (I a123) = C (log.abs $ a123) (signum a123 * (pi/2))
      -- C (log.abs $ (a123 + (sqrt (a123^2 - 1)))) (signum (a123 + (sqrt (a123^2 - 1))) * (pi/2))
      -- for a123 lt (-1) signum evaluates to -1
    | otherwise = C (log.abs $ (a123 + sqrt (a123^2 - 1))) (-pi/2)
    --
  asinh (C a0 a123) =
      -- For C:
      -- log (C a0 a123 + sqrt (C (a0^2 - a123^2 +1) (2*a0*a123)))
      -- Def ==> sqrt (C a0 a123) = C ((sqrt.sqrt $ a0^2 + a123^2) * cos (atan2 a123 a0 / 2)) ((sqrt.sqrt $ a0^2 + a123^2) * sin (atan2 a123 a0 / 2))
      -- log (C a0 a123 + C ((sqrt.sqrt $ (a0^2 - a123^2 +1)^2 + (2*a0*a123)^2) * cos (atan2 (2*a0*a123) (a0^2 - a123^2 +1) / 2)) ((sqrt.sqrt $ (a0^2 - a123^2 +1)^2 + (2*a0*a123)^2) * sin (atan2 (2*a0*a123) (a0^2 - a123^2 +1) / 2)))
      -- log (C (a0 + ((sqrt.sqrt $ (a0^2 - a123^2 +1)^2 + (2*a0*a123)^2) * cos (atan2 (2*a0*a123) (a0^2 - a123^2 +1) / 2))) (a123 + ((sqrt.sqrt $ (a0^2 - a123^2 +1)^2 + (2*a0*a123)^2) * sin (atan2 (2*a0*a123) (a0^2 - a123^2 +1) / 2))))
      -- Def ==> log (C a0 a123) = C (log.sqrt $ a0^2 + a123^2) (atan2 a123 a0)
      -- C (log.sqrt $ (a0 + ((sqrt.sqrt $ (a0^2 - a123^2 +1)^2 + (2*a0*a123)^2) * cos (atan2 (2*a0*a123) (a0^2 - a123^2 +1) / 2)))^2 +
      --               (a123 + ((sqrt.sqrt $ (a0^2 - a123^2 +1)^2 + (2*a0*a123)^2) * sin (atan2 (2*a0*a123) (a0^2 - a123^2 +1) / 2)))^2)
      --   (atan2 (a123 + ((sqrt.sqrt $ (a0^2 - a123^2 +1)^2 + (2*a0*a123)^2) * sin (atan2 (2*a0*a123) (a0^2 - a123^2 +1) / 2)))
      --          (a0 + ((sqrt.sqrt $ (a0^2 - a123^2 +1)^2 + (2*a0*a123)^2) * cos (atan2 (2*a0*a123) (a0^2 - a123^2 +1) / 2))))
      -- Collect like terms:
    let theta = atan2 (2*a0*a123) (a0^2 - a123^2 +1)
        rho = sqrt.sqrt $ (a0^2 - a123^2 +1)^2 + (2*a0*a123)^2
        b0 = a0 + rho * cos (theta/2)
        b123 = a123 + rho * sin (theta/2)
    in C (log (b0^2 + b123^2) / 2) (atan2 b123 b0)
    --
  asinh cliffor = spectraldcmp asinh asinh' cliffor

  --
  acosh (R a0)
    -- log (R a0 + sqrt(R (a0+1)) * sqrt(R (a0-1)))
    | a0 >= 1 = R (acosh a0)
      -- log (R a0 + sqrt(R (a0+1)) * sqrt(R (a0-1)))
      -- log (R a0 + R (sqrt $ a0+1) * R (sqrt $ a0-1))
      -- log (R a0 + R ((sqrt $ a0+1) * (sqrt $ a0-1)))
      -- log (R (a0 + (sqrt $ a0+1) * (sqrt $ a0-1)))
      -- R (log $ a0 + (sqrt $ a0+1) * (sqrt $ a0-1))
      -- R (acosh a0)
      -- Strangely ghc substitutes 'acosh a0' with something like:
      -- R (log $ a0 + (a0 + 1 ) * (sqrt $ (a0 - 1)/(a0 + 1)))
    | a0 >= (-1) = I (atan2 (sqrt $ 1-a0^2) a0) -- This is I because of cancelation of the real component
      -- log (R a0 + sqrt(R (a0+1)) * sqrt(R (a0-1)))
      -- log (R a0 + R (sqrt $ a0+1) * I (sqrt.negate $ a0-1))
      -- log (R a0 + I ((sqrt $ a0+1) * (sqrt.negate $ a0-1)))
      -- log (R a0 + I ((sqrt $ a0+1) * (sqrt $ 1-a0)))
      -- log $ C (a0) ((sqrt $ a0+1) * (sqrt $ 1-a0))
      -- Def log ==> log (C b0 b123) = C (log.sqrt $ b0^2 + b123^2) (atan2 b123 b0)
      -- let b0 = a0
      --     b123 = (sqrt $ a0+1) * (sqrt $ 1-a0) = sqrt $ 1-a0^2
      -- in C (log.sqrt $ b0^2 + b123^2) (atan2 b123 b0)
      -- b123^2 = 1-a0^2
      -- C (log.sqrt $ a0^2 + 1-a0^2) (atan2 (sqrt $ 1-a0^2) a0)
      -- C (log.sqrt $ 1) (atan2 (sqrt $ 1-a0^2) a0)
      -- C 0 (atan2 (sqrt $ 1-a0^2) a0)
      -- I (atan2 (sqrt $ 1-a0^2) a0)
    | otherwise = C (acosh.negate $ a0) pi
      -- log (R a0 + sqrt(R (a0+1)) * sqrt(R (a0-1)))
      -- log (R a0 + I (sqrt.negate $ a0+1) * I (sqrt.negate $ a0-1))
      -- Def ==> (I a123) * (I b123) = R (negate $ a123*b123)
      -- log (R a0 + R (negate $ (sqrt.negate $ a0+1) * (sqrt.negate $ a0-1))
      -- log (R (a0 + (negate $ (sqrt.negate $ a0+1) * (sqrt.negate $ a0-1))))
      -- C (log.negate $ (a0 + (negate $ (sqrt.negate $ a0+1) * (sqrt.negate $ a0-1)))) pi
      -- C (log $ (negate a0 + ((sqrt $ (negate a0)+1) * (sqrt $ (negate a0)-1)))) pi
      -- C (acosh (negate a0)) pi
      --
  acosh (I a123)
      -- log (I a123 + sqrt(C 1 a123) * sqrt(C (-1) a123))
      -- Def ==> sqrt (C a0 a123) =
      --   C ((sqrt.sqrt $ a0^2 + a123^2) * cos (atan2 a123 a0 / 2))
      --      ((sqrt.sqrt $ a0^2 + a123^2) * sin (atan2 a123 a0 / 2))
      -- log (I a123 +
      --      C ((sqrt.sqrt $ 1 + a123^2) * cos (atan2 a123 1 / 2))
      --        ((sqrt.sqrt $ 1 + a123^2) * sin (atan2 a123 1 / 2)) *
      --      C ((sqrt.sqrt $ 1 + a123^2) * cos (atan2 a123 (-1) / 2))
      --        ((sqrt.sqrt $ 1 + a123^2) * sin (atan2 a123 (-1) / 2)) )
      -- Factor out "(sqrt.sqrt $ 1 + a123^2)*"
      -- log (I a123 + R (sqrt.sqrt $ 1 + a123^2) *
      --               C (cos (atan2 a123 1 / 2)) (sin (atan2 a123 1 / 2)) *
      --               R (sqrt.sqrt $ 1 + a123^2) *
      --               C (cos (atan2 a123 (-1) / 2)) (sin (atan2 a123 (-1) / 2)))
      -- Collect both R's and simplify
      -- log (I a123 + (R (sqrt $ 1 + a123^2)) *
      --                C (cos (atan2 a123 1 / 2)) (sin (atan2 a123 1 / 2)) *
      --                C (cos (atan2 a123 (-1) / 2)) (sin (atan2 a123 (-1) / 2)))
      -- Def ==> (C a0 a123) * (C b0 b123) = C (a0*b0 - a123*b123) (a0*b123 + a123*b0)
      -- log (I a123 + R (sqrt $ 1 + a123^2) *
      --               C ((cos (atan2 a123 1 / 2))*(cos (atan2 a123 (-1) / 2)) - (sin (atan2 a123 1 / 2))*(sin (atan2 a123 (-1) / 2)))
      --                 ((cos (atan2 a123 1 / 2))*(sin (atan2 a123 (-1) / 2)) + (sin (atan2 a123 1 / 2))*(cos (atan2 a123 (-1) / 2))) )
      --
      -- Solution now branches for positive and negative a123
      --
      -- For a123 > 0 Substitute (cos (atan2 a123 (-1) / 2)) == (sin (atan2 a123 1 / 2)) AND
      --                         (sin (atan2 a123 (-1) / 2)) == (cos (atan2 a123 1 / 2)) AND
      --                         atan2 a123 1 == atan a123
      -- log (I a123 + R (sqrt $ 1 + a123^2) *
      --               C ((cos (atan a123 / 2))*(sin (atan a123 / 2)) - (sin (atan a123 / 2))*(cos (atan a123 / 2)))
      --                 ((cos (atan a123 / 2))*(cos (atan a123 / 2)) + (sin (atan a123 / 2))*(sin (atan a123 / 2))) )
      -- sin^2 + cos^2 == 1 AND cos*sin - sin*cos == 0 AND Reduce C 0 1 to I 1 AND apply (*) AND apply (+)
      -- log (I (a123 + sqrt (1 + a123^2)))
      -- Def ==> log (I a123) = C (log.abs $ a123) (signum a123 * (pi/2))
      -- C (log.abs $ (a123 + sqrt (1 + a123^2))) (signum (a123 + sqrt (1 + a123^2)) * (pi/2))
      -- With a123 positive Apply signum:
      -- C (log.abs $ (a123 + sqrt (1 + a123^2))) (pi/2)
    | a123 > 0 = C (log.abs $ (a123 + sqrt (1 + a123^2))) (pi/2)
      -- With a123 == 0:
      -- reduce C 0 (pi/2)
      -- I (pi/2)
    | a123 == 0 = I (pi/2)
      -- For a123 < 0 Substitute (cos (atan2 a123 (-1) / 2)) == (negate.sin $ (atan2 a123 1 / 2)) AND
      --                         (sin (atan2 a123 (-1) / 2)) == (negate.cos $ (atan2 a123 1 / 2)) AND
      --                         atan2 a123 1 == atan a123
      -- log (I a123 + R (sqrt $ 1 + a123^2) *
      --               C ((cos (atan2 a123 1 / 2))*(negate.sin $ (atan2 a123 1 / 2)) - (sin (atan2 a123 1 / 2))*(negate.cos $ (atan2 a123 1 / 2)))
      --                 ((cos (atan2 a123 1 / 2))*(negate.cos $ (atan2 a123 1 / 2)) + (sin (atan2 a123 1 / 2))*(negate.sin $ (atan2 a123 1 / 2))) )
      -- Factor negate out AND sin^2 + cos^2 == 1 AND cos*sin - sin*cos == 0 AND Reduce C 0 (-1) to I (-1) AND apply (*) AND apply (+)
      -- log (I (a123 - sqrt (1 + a123^2)))
      -- Def ==> log (I a123) = C (log.abs $ a123) (signum a123 * (pi/2))
      -- C (log.abs $ (a123 - sqrt (1 + a123^2))) (signum (a123 - sqrt (1 + a123^2)) * (pi/2))
      -- With a123 negateive Apply signum:
      -- C (log.abs $ (a123 - sqrt (1 + a123^2))) (-pi/2)
    | otherwise = C (log.abs $ (a123 - sqrt (1 + a123^2))) (-pi/2)
    --
  acosh (C a0 a123) =
      -- log (C a0 a123 + sqrt(C (a0+1) a123) * sqrt(C (a0-1) a123))
      -- Def ==> sqrt (C a0 a123) =
      --   C ((sqrt.sqrt $ a0^2 + a123^2) * cos (atan2 a123 a0 / 2))
      --      ((sqrt.sqrt $ a0^2 + a123^2) * sin (atan2 a123 a0 / 2))
      -- log (C a0 a123 +
      --      C ((sqrt.sqrt $ (a0+1)^2 + a123^2) * cos (atan2 a123 (a0+1) / 2))
      --        ((sqrt.sqrt $ (a0+1)^2 + a123^2) * sin (atan2 a123 (a0+1) / 2)) *
      --      C ((sqrt.sqrt $ (a0-1)^2 + a123^2) * cos (atan2 a123 (a0-1) / 2))
      --        ((sqrt.sqrt $ (a0-1)^2 + a123^2) * sin (atan2 a123 (a0-1) / 2)) )
      -- Factor out the scalar in both Complex numbers
      -- log (C a0 a123 +
      --      R (sqrt.sqrt $ (a0+1)^2 + a123^2) *
      --      C (cos (atan2 a123 (a0+1) / 2)) (sin (atan2 a123 (a0+1) / 2)) *
      --      R (sqrt.sqrt $ (a0-1)^2 + a123^2) *
      --      C (cos (atan2 a123 (a0-1) / 2)) (sin (atan2 a123 (a0-1) / 2)) )
      -- Combine the R terms
      -- log (C a0 a123 +
      --      R (sqrt.sqrt $ ((a0+1)^2 + a123^2) * ((a0-1)^2 + a123^2)) *
      --      C (cos (atan2 a123 (a0+1) / 2)) (sin (atan2 a123 (a0+1) / 2)) *
      --      C (cos (atan2 a123 (a0-1) / 2)) (sin (atan2 a123 (a0-1) / 2)) )
      -- Def ==> (C a0 a123) * (C b0 b123) = C (a0*b0 - a123*b123)
      --                                       (a0*b123 + a123*b0)
      -- log (C a0 a123 +
      --      R (sqrt.sqrt $ ((a0+1)^2 + a123^2) * ((a0-1)^2 + a123^2)) *
      --      C (((cos (atan2 a123 (a0+1) / 2))*(cos (atan2 a123 (a0-1) / 2))) - ((sin (atan2 a123 (a0+1) / 2))*(sin (atan2 a123 (a0-1) / 2))))
      --        (((cos (atan2 a123 (a0+1) / 2))*(sin (atan2 a123 (a0-1) / 2))) + ((sin (atan2 a123 (a0+1) / 2))*(cos (atan2 a123 (a0-1) / 2)))) )
      -- =
      -- log (C a0 a123 +
      --      R (sqrt.sqrt $ ((a0+1)^2 + a123^2) * ((a0-1)^2 + a123^2)) *
      --      C (cos(0.5*(atan2 a123 (a0+1) + atan2 a123 (a0-1))))
      --        (sin(0.5*(atan2 a123 (a0-1) + atan2 a123 (a0+1)))) )
      -- Apply (*)
      -- log (C a0 a123 +
      --      C ((sqrt.sqrt $ ((a0+1)^2 + a123^2) * ((a0-1)^2 + a123^2)) *(cos(0.5*(atan2 a123 (a0+1) + atan2 a123 (a0-1)))))
      --        ((sqrt.sqrt $ ((a0+1)^2 + a123^2) * ((a0-1)^2 + a123^2)) *(sin(0.5*(atan2 a123 (a0-1) + atan2 a123 (a0+1))))) )
      -- Apply (+)
      -- log (C (a0 + (sqrt.sqrt $ ((a0+1)^2 + a123^2) * ((a0-1)^2 + a123^2)) * ((cos(0.5*(atan2 a123 (a0+1) + atan2 a123 (a0-1))))))
      --        (a123 + (sqrt.sqrt $ ((a0+1)^2 + a123^2) * ((a0-1)^2 + a123^2)) * ((sin(0.5*(atan2 a123 (a0-1) + atan2 a123 (a0+1)))))) )
      -- Def ==>  log (C a0 a123) = C (log.sqrt $ a0^2 + a123^2) (atan2 a123 a0)
      -- = C (log.sqrt $ (a0 + (sqrt.sqrt $ ((a0+1)^2 + a123^2) * ((a0-1)^2 + a123^2)) * ((cos(0.5*(atan2 a123 (a0+1) + atan2 a123 (a0-1))))))^2 + (a123 + (sqrt.sqrt $ ((a0+1)^2 + a123^2) * ((a0-1)^2 + a123^2)) * ((sin(0.5*(atan2 a123 (a0-1) + atan2 a123 (a0+1))))))^2) 
      --     (atan2 (a123 + (sqrt.sqrt $ ((a0+1)^2 + a123^2) * ((a0-1)^2 + a123^2)) * ((sin(0.5*(atan2 a123 (a0-1) + atan2 a123 (a0+1)))))) (a0 + (sqrt.sqrt $ ((a0+1)^2 + a123^2) * ((a0-1)^2 + a123^2)) * ((cos(0.5*(atan2 a123 (a0+1) + atan2 a123 (a0-1)))))))
      -- Collect like terms:
    let theta = atan2 a123 (a0+1) + atan2 a123 (a0-1)
        rho = sqrt.sqrt $ ((a0+1)^2 + a123^2) * ((a0-1)^2 + a123^2)
        b0 = a0 + rho * cos(theta/2)
        b123 = a123 + rho * sin(theta/2)
    in C (log (b0^2 + b123^2) / 2) (atan2 b123 b0)
    --
  acosh cliffor = spectraldcmp acosh acosh' cliffor

  --
  atanh (R a0)
      -- = 0.5*log (R (1+a0)) - 0.5*log (R (1-a0))
      -- = (R ((0.5*).log $ 1+a0)) - (C ((0.5*).log.negate $ 1-a0) (pi/2))
      -- = C (((0.5*).log $ 1+a0) - ((0.5*).log.negate $ 1-a0)) (-pi/2)
      -- = C (0.5*((log $ 1+a0) - (log $ a0-1))) (-pi/2)
    | a0 > 1 = C ((log (1+a0) - log (a0-1))/2) (-pi/2)
      -- = 0.5 * (log (R (1+a0)) - log (R (1-a0)))
      -- = 0.5*(R (log $ 1+a0) - R (log $ 1-a0))
      -- = R (0.5*(log $ 1+a0) - 0.5*(log $ 1-a0))
      -- = R (atanh a0)
    | a0 >= (-1) = R (atanh a0)
      -- = 0.5 * (log (R (1+a0)) - log (R (1-a0)))
      -- = (C ((0.5*).log.negate $ 1+a0) (pi/2)) - (R ((0.5*).log $ 1-a0))
      -- = C (((0.5*).log.negate $ 1+a0) - ((0.5*).log $ 1-a0)) (pi/2)
      -- = C (0.5*((log.negate $ 1+a0) - (log $ 1-a0))) (pi/2)
    | otherwise = C (((log.negate $ 1+a0) - log (1-a0))/2) (pi/2)
    --
    -- For I:
    -- = 0.5 * (log (C 1 a123) - log (C 1 (-a123)))
    -- = I (atan a123)
  atanh (I a123)
    | a123 == 0 = R 0
    | otherwise = I (atan a123)
    -- = 0.5 * (log (C (1+a0) a123) - log (C (1-a0) (-a123)))
    -- Def log ==> log (C a0 a123) = C (log.sqrt $ a0^2 + a123^2) (atan2 a123 a0)
    -- log (C (1+a0) a123) = C (log.sqrt $ (1+a0)^2 + a123^2) (atan2 a123 (1+a0))
    -- log (C (1-a0) (-a123)) = C (log.sqrt $ (1-a0)^2 + (-a123)^2) (atan2 (-a123) (1-a0))
    -- = C (((0.5*).log.sqrt $ (1+a0)^2 + a123^2) - ((0.5*).log.sqrt $ (1-a0)^2 + a123^2)) (0.5*((atan2 a123 (1+a0)) - (atan2 (-a123) (1-a0))))
    -- C (((log $ (1+a0)^2 + a123^2) - (log $ (1-a0)^2 + a123^2))/4) (((atan2 a123 (1-a0)) + (atan2 a123 (1+a0)))/2)
  atanh (C a0 a123) = C ((log ((1+a0)^2 + a123^2) - log ((1-a0)^2 + a123^2))/4) ((atan2 a123 (1-a0) + atan2 a123 (1+a0))/2)
  --
  atanh cliffor = spectraldcmp atanh atanh' cliffor



-- |'lsv' the littlest singular value. Useful for testing for invertability.
lsv :: Cl3 -> Cl3
lsv (R a0) = R (abs a0) -- absolute value of a real number
lsv (V3 a1 a2 a3) = R (sqrt (a1^2 + a2^2 + a3^2)) -- magnitude of a vector
lsv (BV a23 a31 a12) = R (sqrt (a23^2 + a31^2 + a12^2)) -- magnitude of a bivector
lsv (I a123) = R (abs a123)
lsv (PV a0 a1 a2 a3) = R (loDisc a0 a1 a2 a3)
lsv (TPV a23 a31 a12 a123) = R (loDisc a123 a23 a31 a12)
lsv (H a0 a23 a31 a12) = R (sqrt (a0^2 + a23^2 + a31^2 + a12^2))
lsv (C a0 a123) = R (sqrt (a0^2 + a123^2)) -- magnitude of a complex number
lsv (BPV a1 a2 a3 a23 a31 a12) =
  let x = negate.sqrt $ (a1*a31 - a2*a23)^2 + (a1*a12 - a3*a23)^2 + (a2*a12 - a3*a31)^2 -- core was duplicating this computation added let to hopefully reduce the duplication
      y = a1^2 + a23^2 + a2^2 + x + a31^2 + a3^2 + a12^2 + x -- attempted to balance out the sum of several positives with a negitive before the next sum of positives and negitive
  in if y <= tol' -- gaurd for numerical errors, y could be negative with large enough biparavectors
     then R 0
     else R (sqrt y)
lsv (ODD a1 a2 a3 a123) = R (sqrt (a1^2 + a2^2 + a3^2 + a123^2))
lsv (APS a0 a1 a2 a3 a23 a31 a12 a123) =
  let x = negate.sqrt $ (a0*a1 + a123*a23)^2 + (a0*a2 + a123*a31)^2 + (a0*a3 + a123*a12)^2 +
                        (a2*a12 - a3*a31)^2 + (a3*a23 - a1*a12)^2 + (a1*a31 - a2*a23)^2 -- core was duplicating this computation added let to hopefully reduce the duplication
      y = a0^2 + a1^2 + a2^2 + a3^2 + x + a23^2 + a31^2 + a12^2 + a123^2 + x -- attempted to balance out the sum of several positives with a negitive before the next sum of positives and negitive
  in if y <= tol' -- gaurd for numerical errors, y could be negative with large enough cliffors
     then R 0
     else R (sqrt y)


-- | 'loDisc' The Lower Discriminant for Paravectors and Triparavectors, real and imagninary portions of APS
loDisc :: Double -> Double -> Double -> Double -> Double
loDisc v0 v1 v2 v3 =
  let sumsqs = v1^2 + v2^2 + v3^2
      x = negate $ abs v0 * sqrt sumsqs
      y = v0^2 + x + sumsqs + x
  in if y <= tol' -- gaurd for numerical errors, y could be negative with large enough paravectors
     then 0
     else sqrt y


-- | 'spectraldcmp' the spectral decomposition of a function to calculate analytic functions of cliffors in Cl(3,0).
-- This function requires the desired function's R, I, and C instances to be calculated and the function's derivative.
-- If multiple functions are being composed, its best to pass the composition of the funcitons
-- to this function and the derivative to this function.  Any function with a Taylor Series
-- approximation should be able to be used.  A real, imaginary, and complex version of the function to be decomposed
-- must be provided and spectraldcmp will handle the case for an arbitrary Cliffor.
-- 
-- It may be possible to add, in the future, a RULES pragma like:
--
-- > "spectral decomposition function composition"
-- > forall f f' g g' cliff.
-- > spectraldcmp f f' (spectraldcmp g g' cliff) = spectraldcmp (f.g) (f'.g') cliff
-- 
-- 
spectraldcmp :: (Cl3 -> Cl3) -> (Cl3 -> Cl3) -> Cl3 -> Cl3
spectraldcmp fun fun' (reduce -> cliffor) = dcmp cliffor
  where
    dcmp r@R{} = fun r
    dcmp i@I{} = fun i
    dcmp c@C{} = fun c
    dcmp v@V3{} = spectraldcmpSpecial toR fun v -- spectprojR fun v
    dcmp pv@PV{} = spectraldcmpSpecial toR fun pv -- spectprojR fun pv
    dcmp bv@BV{} = spectraldcmpSpecial toI fun bv -- spectprojI fun bv
    dcmp tpv@TPV{} = spectraldcmpSpecial toI fun tpv -- spectprojI fun tpv
    dcmp h@H{} = spectraldcmpSpecial toC fun h -- spectprojC fun h
    dcmp od@ODD{} = spectraldcmpSpecial toC fun od -- spectprojC fun od
    dcmp cliff
      | hasNilpotent cliff = jordan toC fun fun' cliff  -- jordan normal form Cl3 style
      | isColinear cliff = spectraldcmpSpecial toC fun cliff -- spectprojC fun bpv
      | otherwise =                               -- transform it so it will be colinear
          let (BPV a1 a2 a3 a23 a31 a12) = toBPV cliff
              boost = boost2colinear a1 a2 a3 a23 a31 a12
          in boost * spectraldcmpSpecial toC fun (bar boost * cliff * boost) * bar boost -- v * spectprojC fun d * v_bar
--


-- | 'jordan' does a Cl(3,0) version of the decomposition into Jordan Normal Form and Matrix Function Calculation
-- The intended use is for calculating functions for cliffors with vector parts simular to Nilpotent.
-- It is a helper function for 'spectraldcmp'.  It is fortunate because eigen decomposition doesn't
-- work with elements with nilpotent content, so it fills the gap.
jordan :: (Cl3 -> Cl3) -> (Cl3 -> Cl3) -> (Cl3 -> Cl3) -> Cl3 -> Cl3
jordan toSpecial fun fun' cliffor =
  let eigs = toSpecial cliffor
  in fun eigs + fun' eigs * toBPV cliffor

-- | 'spectraldcmpSpecial' helper function for with specialization for real, imaginary, or complex eigenvalues.
-- To specialize for Reals pass 'toR', to specialize for Imaginary pass 'toI', to specialize for Complex pass 'toC'
spectraldcmpSpecial :: (Cl3 -> Cl3) -> (Cl3 -> Cl3) -> Cl3 -> Cl3
spectraldcmpSpecial toSpecial function cliffor =
  let (p,p_bar,eig1,eig2) = projEigs toSpecial cliffor
  in function eig1 * p + function eig2 * p_bar



-- | 'eigvals' calculates the eignenvalues of the cliffor.
-- This is useful for determining if a cliffor is the pole
-- of a function.
eigvals :: Cl3 -> (Cl3,Cl3)
eigvals (reduce -> cliffor) = eigv cliffor
  where
    eigv r@R{} = dup r
    eigv i@I{} = dup i
    eigv c@C{} = dup c
    eigv v@V3{} = eigvalsSpecial toR v -- eigvalsR v
    eigv pv@PV{} = eigvalsSpecial toR pv -- eigvalsR pv
    eigv bv@BV{} = eigvalsSpecial toI bv -- eigvalsI bv
    eigv tpv@TPV{} = eigvalsSpecial toI tpv -- eigvalsI tpv
    eigv h@H{} = eigvalsSpecial toC h -- eigvalsC h
    eigv od@ODD{} = eigvalsSpecial toC od -- eigvalsC od
    eigv cliff
      | hasNilpotent cliff = dup.reduce.toC $ cliff  -- this case is actually nilpotent
      | isColinear cliff = eigvalsSpecial toC cliff  -- eigvalsC bpv
      | otherwise =                           -- transform it so it will be colinear
          let (BPV a1 a2 a3 a23 a31 a12) = toBPV cliff
              boost = boost2colinear a1 a2 a3 a23 a31 a12
          in eigvalsSpecial toC (bar boost * cliff * boost) -- eigvalsC d
--


dup :: Cl3 -> (Cl3,Cl3)
dup cliff = (cliff, cliff)

-- | 'eigvalsSpecial' helper function to calculate Eigenvalues
eigvalsSpecial :: (Cl3 -> Cl3) -> Cl3 -> (Cl3,Cl3)
eigvalsSpecial toSpecial cliffor =
  let (_,_,eig1,eig2) = projEigs toSpecial cliffor
  in (eig1,eig2)


-- | 'project' makes a projector based off of the vector content of the Cliffor.
project :: Cl3 -> Cl3  -- PV<:Cl3
project R{} = PV 0.5 0 0 0.5   -- default to e3 direction
project I{} = PV 0.5 0 0 0.5   -- default to e3 direction
project C{} = PV 0.5 0 0 0.5   -- default to e3 direction
project (V3 a1 a2 a3) = triDProj a1 a2 a3   -- proj v@V3{} = 0.5 + 0.5*signum v
project (PV _ a1 a2 a3) = triDProj a1 a2 a3   -- proj pv@PV{} = 0.5 + 0.5*(signum.toV3 $ pv)
project (ODD a1 a2 a3 _) = triDProj a1 a2 a3   -- od@ODD{} = 0.5 + 0.5*(signum.toV3 $ od)
project (BV a23 a31 a12) = triDProj a23 a31 a12   -- bv@BV{} = 0.5 + 0.5*(mIx.signum $ bv)
project (H _ a23 a31 a12) = triDProj a23 a31 a12   -- h@H{} = 0.5 + 0.5*(mIx.signum.toBV $ h)
project (TPV a23 a31 a12 _) = triDProj a23 a31 a12   -- tpv@TPV{} = 0.5 + 0.5*(mIx.signum.toBV $ tpv)
project (BPV a1 a2 a3 a23 a31 a12) = biTriDProj a1 a2 a3 a23 a31 a12
project (APS _ a1 a2 a3 a23 a31 a12 _) = biTriDProj a1 a2 a3 a23 a31 a12



-- If Dot product is negative or zero we have a problem, if it is zero
-- it either the vector or bivector par is zero or they are orthognal
-- if the dot product is negative the vectors could be antiparallel
biTriDProj :: Double -> Double -> Double -> Double -> Double -> Double -> Cl3  -- PV<:Cl3
biTriDProj a1 a2 a3 a23 a31 a12 =
  let v3Mag = sqrt $ a1^2 + a2^2 + a3^2
      v3MagltTol = v3Mag < tol'
      halfInvV3Mag = recip v3Mag / 2
      bvMag = sqrt $ a23^2 + a31^2 + a12^2
      bvMagltTol = bvMag < tol'
      halfInvBVMag = recip bvMag / 2
      dotPos = (a1*a23) + (a2*a31) + (a3*a12) >= 0
      b1 = a1 + a23
      b2 = a2 + a31
      b3 = a3 + a12
      bHalfInvMag = (/2).recip.sqrt $ b1^2 + b2^2 + b3^2
      c1 = a1 - a23
      c2 = a2 - a31
      c3 = a3 - a12
      cHalfInvMag = (/2).recip.sqrt $ c1^2 + c2^2 + c3^2
  in if | v3MagltTol && bvMagltTol -> PV 0.5 0 0 0.5
        | bvMagltTol -> PV 0.5 (halfInvV3Mag * a1) (halfInvV3Mag * a2) (halfInvV3Mag * a3)
        | v3MagltTol -> PV 0.5 (halfInvBVMag * a23) (halfInvBVMag * a31) (halfInvBVMag * a12)
        | dotPos -> PV 0.5 (bHalfInvMag * b1) (bHalfInvMag * b2) (bHalfInvMag * b3)
        | otherwise -> PV 0.5 (cHalfInvMag * c1) (cHalfInvMag * c2) (cHalfInvMag * c3)


-- | 'triDProj' a single 3 dimensional vector grade to a projector
triDProj :: Double -> Double -> Double -> Cl3  -- PV<:Cl3
triDProj v1 v2 v3 =
  let mag = sqrt $ v1^2 + v2^2 + v3^2
      halfInvMag = recip mag / 2
  in if mag == 0
     then PV 0.5 0 0 0.5
     else PV 0.5 (halfInvMag * v1) (halfInvMag * v2) (halfInvMag * v3)


-- | 'boost2colinear' calculates a boost that is perpendicular to both the vector and bivector
-- components of the cliffor, that will mix the vector and bivector parts such that the vector and bivector
-- parts become colinear. This function is a simularity transform such that:
--
-- > cliffor = boost * colinear * bar boost
--
-- and returns the boost given the inputs.  First the boost must be calculated
-- and then
--
-- > colinear = bar boost * cliffor * boost
--
-- and colinear will have colinear vector and bivector parts of the cliffor.
-- This is somewhat simular to finding the drift frame for a static electromagnetic field.
--
-- > v = toV3 cliffor  -- extract the vector
-- > bv = mIx.toBV $ cliffor  -- extract the bivector and turn it into a vector
-- > invariant = ((2*).mIx.toBV $ v * bv) / (toR (v^2) + toR (bv^2))
-- > boost = spectraldcmpSpecial toR (exp.(/4).atanh) invariant
--
boost2colinear :: Double -> Double -> Double -> Double -> Double -> Double -> Cl3  -- PV<:Cl3
boost2colinear a1 a2 a3 a23 a31 a12 =
  let scale = recip $ a1^2 + a2^2 + a3^2 + a23^2 + a31^2 + a12^2
      b1 = scale * (a2*a12 - a3*a31)
      b2 = scale * (a3*a23 - a1*a12)
      b3 = scale * (a1*a31 - a2*a23)
      eig1 = (2*).sqrt $ b1^2 + b2^2 + b3^2
      eig2 = negate eig1
      transEig1 = exp.(/4).atanh $ eig1
      transEig2 = exp.(/4).atanh $ eig2
      sumTransEigs = (transEig1 - transEig2) * recip eig1
  in PV (0.5 * (transEig1 + transEig2)) (sumTransEigs * b1) (sumTransEigs * b2) (sumTransEigs * b3)


-- | 'isColinear' takes a Cliffor and determines if either the vector part or the bivector part are
-- zero or both aligned in the same direction.
isColinear :: Cl3 -> Bool
isColinear R{} = True
isColinear V3{} = True
isColinear BV{} = True
isColinear I{} = True
isColinear PV{} = True
isColinear H{} = True
isColinear C{} = True
isColinear ODD{} = True
isColinear TPV{} = True
isColinear (BPV a1 a2 a3 a23 a31 a12) = colinearHelper a1 a2 a3 a23 a31 a12
isColinear (APS _ a1 a2 a3 a23 a31 a12 _) = colinearHelper a1 a2 a3 a23 a31 a12

colinearHelper :: Double -> Double -> Double -> Double -> Double -> Double -> Bool
colinearHelper a1 a2 a3 a23 a31 a12 =
  let magV3 = sqrt $ a1^2 + a2^2 + a3^2
      invMagV3 = recip magV3
      magBV = sqrt $ a23^2 + a31^2 + a12^2
      invMagBV = recip magBV
      crss = sqrt (((invMagV3 * a2)*(invMagBV * a12) - (invMagV3 * a3)*(invMagBV * a31))^2 +
                   ((invMagV3 * a3)*(invMagBV * a23) - (invMagV3 * a1)*(invMagBV * a12))^2 +
                   ((invMagV3 * a1)*(invMagBV * a31) - (invMagV3 * a2)*(invMagBV * a23))^2)
  in magV3 == 0 ||     -- Zero Vector
     magBV == 0 ||     -- Zero Bivector
     crss <= tol'      -- Orthoganl part is zero-ish


-- | 'hasNilpotent' takes a Cliffor and determines if the vector part and the bivector part are
-- orthoganl and equal in magnitude, i.e. that it is simular to a nilpotent BPV.
hasNilpotent :: Cl3 -> Bool
hasNilpotent R{} = False
hasNilpotent V3{} = False
hasNilpotent BV{} = False
hasNilpotent I{} = False
hasNilpotent PV{} = False
hasNilpotent H{} = False
hasNilpotent C{} = False
hasNilpotent ODD{} = False
hasNilpotent TPV{} = False
hasNilpotent (BPV a1 a2 a3 a23 a31 a12) = nilpotentHelper a1 a2 a3 a23 a31 a12
hasNilpotent (APS _ a1 a2 a3 a23 a31 a12 _) = nilpotentHelper a1 a2 a3 a23 a31 a12

nilpotentHelper :: Double -> Double -> Double -> Double -> Double -> Double -> Bool
nilpotentHelper a1 a2 a3 a23 a31 a12 =
  let magV3 = sqrt $ a1^2 + a2^2 + a3^2
      invMagV3 = recip magV3
      magBV = sqrt $ a23^2 + a31^2 + a12^2
      invMagBV = recip magV3
      magDiff = abs (magV3 - magBV)
      b1 = invMagV3 * a1
      b2 = invMagV3 * a2
      b3 = invMagV3 * a3
      b23 = invMagBV * a23
      b31 = invMagBV * a31
      b12 = invMagBV * a12
      c0 = b1*b1 + b2*b2 + b3*b3 - b23*b23 - b31*b31 - b12*b12
      c1 = b12*b2 - b2*b12 + b3*b31 - b31*b3
      c2 = b1*b12 - b12*b1 - b3*b23 + b23*b3
      c3 = b31*b1 - b1*b31 + b2*b23 - b23*b2
      c23 = b2*b3 - b3*b2 - b31*b12 + b12*b31
      c31 = b3*b1 - b1*b3 + b23*b12 - b12*b23
      c12 = b1*b2 - b2*b1 - b23*b31 + b31*b23
      c123 = b1*b23 + b23*b1 + b2*b31 + b31*b2 + b3*b12 + b12*b3
      x = sqrt ((c0*c1 + c123*c23)^2 + (c0*c2 + c123*c31)^2 + (c0*c3 + c123*c12)^2 +
                (c2*c12 - c3*c31)^2 + (c3*c23 - c1*c12)^2 + (c1*c31 - c2*c23)^2)
      sqMag = sqrt (c0^2 + c1^2 + c2^2 + c3^2 + c23^2 + c31^2 + c12^2 + c123^2 + x + x)
  in magV3 /= 0 &&          -- Non-Zero Vector Part
     magBV /= 0 &&          -- Non-Zero Bivector Part
     magDiff <= tol' &&     -- Vector and Bivector are Equal Magnitude
     sqMag <= tol'          -- It's non-zero but squares to zero


-- | 'projEigs' function returns complementary projectors and eigenvalues for a Cliffor with specialization.
-- The Cliffor at this point is allready colinear and the Eigenvalue is known to be real, imaginary, or complex.
projEigs :: (Cl3 -> Cl3) -> Cl3 -> (Cl3,Cl3,Cl3,Cl3)
projEigs toSpecial cliffor =
  let p = project cliffor
      p_bar = bar p
      eig1 = 2 * toSpecial (p * cliffor * p)
      eig2 = 2 * toSpecial (p_bar * cliffor * p_bar)
  in (p,p_bar,eig1,eig2)

-- | 'reduce' function reduces the number of grades in a specialized Cliffor if they
-- are zero-ish
reduce :: Cl3 -> Cl3
reduce cliff
  | abs cliff <= tol = R 0
  | otherwise = go_reduce cliff
    where
      go_reduce r@R{} = r
      go_reduce v@V3{} = v
      go_reduce bv@BV{} = bv
      go_reduce i@I{} = i
      go_reduce pv@PV{}
        | abs (toV3 pv) <= tol = toR pv
        | abs (toR pv) <= tol = toV3 pv
        | otherwise = pv
      go_reduce h@H{}
        | abs (toBV h) <= tol = toR h
        | abs (toR h) <= tol = toBV h
        | otherwise = h
      go_reduce c@C{}
        | abs (toI c) <= tol = toR c
        | abs (toR c) <= tol = toI c
        | otherwise = c
      go_reduce bpv@BPV{}
        | abs (toBV bpv) <= tol = toV3 bpv
        | abs (toV3 bpv) <= tol = toBV bpv
        | otherwise = bpv
      go_reduce od@ODD{}
        | abs (toI od) <= tol = toV3 od
        | abs (toV3 od) <= tol = toI od
        | otherwise = od
      go_reduce tpv@TPV{}
        | abs (toBV tpv) <= tol = toI tpv
        | abs (toI tpv) <= tol = toBV tpv
        | otherwise = tpv
      go_reduce aps@APS{}
        | abs (toBPV aps) <= tol = go_reduce (toC aps)
        | abs (toODD aps) <= tol = go_reduce (toH aps)
        | abs (toTPV aps) <= tol = go_reduce (toPV aps)
        | abs (toC aps) <= tol = go_reduce (toBPV aps)
        | abs (toH aps) <= tol = go_reduce (toODD aps)
        | abs (toPV aps) <= tol = go_reduce (toTPV aps)
        | otherwise = aps


-- | 'mIx' a more effecient '\x -> I (-1) * x' typically useful for converting a
-- Bivector to a Vector in the same direction. Related to Hodge Dual and/or
-- Inverse Hodge Star.
mIx :: Cl3 -> Cl3
mIx (R a0) = I (negate a0)
mIx (V3 a1 a2 a3) = BV (negate a1) (negate a2) (negate a3)
mIx (BV a23 a31 a12) = V3 a23 a31 a12
mIx (I a123) = R a123
mIx (PV a0 a1 a2 a3) = TPV (negate a1) (negate a2) (negate a3) (negate a0)
mIx (H a0 a23 a31 a12) = ODD a23 a31 a12 (negate a0)
mIx (C a0 a123) = C a123 (negate a0)
mIx (BPV a1 a2 a3 a23 a31 a12) = BPV a23 a31 a12 (negate a1) (negate a2) (negate a3)
mIx (ODD a1 a2 a3 a123) = H a123 (negate a1) (negate a2) (negate a3)
mIx (TPV a23 a31 a12 a123) = PV a123 a23 a31 a12
mIx (APS a0 a1 a2 a3 a23 a31 a12 a123) = APS a123 a23 a31 a12 (negate a1) (negate a2) (negate a3) (negate a0)

-- | 'timesI' is a more effecient '\x -> I 1 * x'
timesI :: Cl3 -> Cl3
timesI (R a0) = I a0
timesI (V3 a1 a2 a3) = BV a1 a2 a3
timesI (BV a23 a31 a12) = V3 (negate a23) (negate a31) (negate a12)
timesI (I a123) = R (negate a123)
timesI (PV a0 a1 a2 a3) = TPV a1 a2 a3 a0
timesI (H a0 a23 a31 a12) = ODD (negate a23) (negate a31) (negate a12) a0
timesI (C a0 a123) = C (negate a123) a0
timesI (BPV a1 a2 a3 a23 a31 a12) = BPV (negate a23) (negate a31) (negate a12) a1 a2 a3
timesI (ODD a1 a2 a3 a123) = H (negate a123) a1 a2 a3
timesI (TPV a23 a31 a12 a123) = PV (negate a123) (negate a23) (negate a31) (negate a12)
timesI (APS a0 a1 a2 a3 a23 a31 a12 a123) = APS (negate a123) (negate a23) (negate a31) (negate a12) a1 a2 a3 a0

-- | 'abssignum' is a more effecient '\cl3 -> (abs cl3, signum cl3)'
-- So 'abs' is always R and 'signum' is the same type of constructor as the input
-- 'signum' is the element divided by its largest singular value 'abs'
abssignum :: Cl3 -> (Cl3,Cl3)
abssignum cl3 =
  let (R m0) = absolute cl3
  in if m0 == 0
     then (R 0, R 0) -- (abs 0 == 0, signum 0 == 0)
     else (R m0, cl3/(R m0))

absolute :: Cl3 -> Cl3
absolute (R a0) = R (abs a0)
absolute (V3 a1 a2 a3) = let m = rss3 a1 a2 a3 in R m
absolute (BV a23 a31 a12) = let m = rss3 a23 a31 a12 in R m
absolute (I a123) = R (abs a123)
absolute (PV a0 a1 a2 a3) = let m = reimMag a0 a1 a2 a3 in R m
absolute (H a0 a23 a31 a12) = let m = rss4 a0 a23 a31 a12 in R m
absolute (C a0 a123) = let m = rss2 a0 a123 in R m
absolute (BPV a1 a2 a3 a23 a31 a12) = let mag0 = rss3 (a1*a31 - a2*a23) (a1*a12 - a3*a23) (a2*a12 - a3*a31)
                                          m = sqrt $ a1^2 + a23^2 + a2^2 + a31^2 + a3^2 + a12^2 + 2*mag0 in R m
absolute (ODD a1 a2 a3 a123) = let m = rss4 a1 a2 a3 a123 in R m
absolute (TPV a23 a31 a12 a123) = let m = reimMag a123 a23 a31 a12 in R m
absolute (APS a0 a1 a2 a3 a23 a31 a12 a123) = let mag0 = sqrt $ (a0*a1 + a123*a23)^2 + (a0*a2 + a123*a31)^2 + (a0*a3 + a123*a12)^2 + (a2*a12 - a3*a31)^2 + (a3*a23 - a1*a12)^2 + (a1*a31 - a2*a23)^2
                                                  m = sqrt $ a0^2 + a1^2 + a2^2 + a3^2 + a23^2 + a31^2 + a12^2 + a123^2 + 2*mag0 in R m

rss2 :: Double -> Double -> Double
rss2 a0 a123 = sqrt $ a0^2 + a123^2

rss3 :: Double -> Double -> Double -> Double
rss3 x y z = sqrt $ x^2 + y^2 + z^2

rss4 :: Double -> Double -> Double -> Double -> Double
rss4 t x y z = sqrt $ t^2 + x^2 + y^2 + z^2



#ifdef O_LIQUID
tol :: Cl3
tol = R 0

tol' :: Double
tol' = 0
#else
-- | 'tol' currently 128*eps
tol :: Cl3
{-# INLINE tol #-}
tol = R 1.4210854715202004e-14

tol' :: Double
{-# INLINE tol' #-}
tol' = 1.4210854715202004e-14
#endif

-- | 'bar' is a Clifford Conjugate, the vector grades are negated
bar :: Cl3 -> Cl3
bar (R a0) = R a0
bar (V3 a1 a2 a3) = V3 (negate a1) (negate a2) (negate a3)
bar (BV a23 a31 a12) = BV (negate a23) (negate a31) (negate a12)
bar (I a123) = I a123
bar (PV a0 a1 a2 a3) = PV a0 (negate a1) (negate a2) (negate a3)
bar (H a0 a23 a31 a12) = H a0 (negate a23) (negate a31) (negate a12)
bar (C a0 a123) = C a0 a123
bar (BPV a1 a2 a3 a23 a31 a12) = BPV (negate a1) (negate a2) (negate a3) (negate a23) (negate a31) (negate a12)
bar (ODD a1 a2 a3 a123) = ODD (negate a1) (negate a2) (negate a3) a123
bar (TPV a23 a31 a12 a123) = TPV (negate a23) (negate a31) (negate a12) a123
bar (APS a0 a1 a2 a3 a23 a31 a12 a123) = APS a0 (negate a1) (negate a2) (negate a3) (negate a23) (negate a31) (negate a12) a123

-- | 'dag' is the Complex Conjugate, the imaginary grades are negated
dag :: Cl3 -> Cl3
dag (R a0) = R a0
dag (V3 a1 a2 a3) = V3 a1 a2 a3
dag (BV a23 a31 a12) = BV (negate a23) (negate a31) (negate a12)
dag (I a123) = I (negate a123)
dag (PV a0 a1 a2 a3) =  PV a0 a1 a2 a3
dag (H a0 a23 a31 a12) = H a0 (negate a23) (negate a31) (negate a12)
dag (C a0 a123) = C a0 (negate a123)
dag (BPV a1 a2 a3 a23 a31 a12) = BPV a1 a2 a3 (negate a23) (negate a31) (negate a12)
dag (ODD a1 a2 a3 a123) = ODD a1 a2 a3 (negate a123)
dag (TPV a23 a31 a12 a123) = TPV (negate a23) (negate a31) (negate a12) (negate a123)
dag (APS a0 a1 a2 a3 a23 a31 a12 a123) = APS a0 a1 a2 a3 (negate a23) (negate a31) (negate a12) (negate a123)

----------------------------------------------------------------------------------------------------------------
-- the to... functions provide a lossy cast from one Cl3 constructor to another
---------------------------------------------------------------------------------------------------------------
-- | 'toR' takes any Cliffor and returns the R portion
toR :: Cl3 -> Cl3
toR (R a0) = R a0
toR V3{} = R 0
toR BV{} = R 0
toR I{} = R 0
toR (PV a0 _ _ _) = R a0
toR (H a0 _ _ _) = R a0
toR (C a0 _) = R a0
toR BPV{} = R 0
toR ODD{} = R 0
toR TPV{} = R 0
toR (APS a0 _ _ _ _ _ _ _) = R a0

-- | 'toV3' takes any Cliffor and returns the V3 portion
toV3 :: Cl3 -> Cl3
toV3 R{} = V3 0 0 0
toV3 (V3 a1 a2 a3) = V3 a1 a2 a3
toV3 BV{} = V3 0 0 0
toV3 I{} = V3 0 0 0
toV3 (PV _ a1 a2 a3) = V3 a1 a2 a3
toV3 H{} = V3 0 0 0
toV3 C{} = V3 0 0 0
toV3 (BPV a1 a2 a3 _ _ _) = V3 a1 a2 a3
toV3 (ODD a1 a2 a3 _) = V3 a1 a2 a3
toV3 TPV{} = V3 0 0 0
toV3 (APS _ a1 a2 a3 _ _ _ _) = V3 a1 a2 a3

-- | 'toBV' takes any Cliffor and returns the BV portion
toBV :: Cl3 -> Cl3
toBV R{} = BV 0 0 0
toBV V3{} = BV 0 0 0
toBV (BV a23 a31 a12) = BV a23 a31 a12
toBV I{} = BV 0 0 0
toBV PV{} = BV 0 0 0
toBV (H _ a23 a31 a12) = BV a23 a31 a12
toBV C{} = BV 0 0 0
toBV (BPV _ _ _ a23 a31 a12) = BV a23 a31 a12
toBV ODD{} = BV 0 0 0
toBV (TPV a23 a31 a12 _) = BV a23 a31 a12
toBV (APS _ _ _ _ a23 a31 a12 _) = BV a23 a31 a12

-- | 'toI' takes any Cliffor and returns the I portion
toI :: Cl3 -> Cl3
toI R{} = I 0
toI V3{} = I 0
toI BV{} = I 0
toI (I a123) = I a123
toI PV{} = I 0
toI H{} = I 0
toI (C _ a123) = I a123
toI BPV{} = I 0
toI (ODD _ _ _ a123) = I a123
toI (TPV _ _ _ a123) = I a123
toI (APS _ _ _ _ _ _ _ a123) = I a123

-- | 'toPV' takes any Cliffor and returns the PV poriton
toPV :: Cl3 -> Cl3
toPV (R a0) = PV a0 0 0 0
toPV (V3 a1 a2 a3) = PV 0 a1 a2 a3
toPV BV{} = PV 0 0 0 0
toPV I{} = PV 0 0 0 0
toPV (PV a0 a1 a2 a3) = PV a0 a1 a2 a3
toPV (H a0 _ _ _) = PV a0 0 0 0
toPV (C a0 _) = PV a0 0 0 0
toPV (BPV a1 a2 a3 _ _ _) = PV 0 a1 a2 a3
toPV (ODD a1 a2 a3 _) = PV a1 a2 a3 0
toPV TPV{} = PV 0 0 0 0
toPV (APS a0 a1 a2 a3 _ _ _ _) = PV a0 a1 a2 a3

-- | 'toH' takes any Cliffor and returns the H portion
toH :: Cl3 -> Cl3
toH (R a0) = H a0 0 0 0
toH V3{} = H 0 0 0 0
toH (BV a23 a31 a12) = H 0 a23 a31 a12
toH (I _) = H 0 0 0 0
toH (PV a0 _ _ _) = H a0 0 0 0
toH (H a0 a23 a31 a12) = H a0 a23 a31 a12
toH (C a0 _) = H a0 0 0 0
toH (BPV _ _ _ a23 a31 a12) = H 0 a23 a31 a12
toH ODD{} = H 0 0 0 0
toH (TPV a23 a31 a12 _) = H 0 a23 a31 a12
toH (APS a0 _ _ _ a23 a31 a12 _) = H a0 a23 a31 a12

-- | 'toC' takes any Cliffor and returns the C portion
toC :: Cl3 -> Cl3
toC (R a0) = C a0 0
toC V3{} = C 0 0
toC BV{} = C 0 0
toC (I a123) = C 0 a123
toC (PV a0 _ _ _) = C a0 0
toC (H a0 _ _ _) = C a0 0
toC (C a0 a123) = C a0 a123
toC BPV{} = C 0 0
toC (ODD _ _ _ a123) = C 0 a123
toC (TPV _ _ _ a123) = C 0 a123
toC (APS a0 _ _ _ _ _ _ a123) = C a0 a123

-- | 'toBPV' takes any Cliffor and returns the BPV portion
toBPV :: Cl3 -> Cl3
toBPV R{} = BPV 0 0 0 0 0 0
toBPV (V3 a1 a2 a3) = BPV a1 a2 a3 0 0 0
toBPV (BV a23 a31 a12) = BPV 0 0 0 a23 a31 a12
toBPV I{} = BPV 0 0 0 0 0 0
toBPV (PV _ a1 a2 a3) = BPV a1 a2 a3 0 0 0
toBPV (H _ a23 a31 a12) = BPV 0 0 0 a23 a31 a12
toBPV C{} = BPV 0 0 0 0 0 0
toBPV (BPV a1 a2 a3 a23 a31 a12) = BPV a1 a2 a3 a23 a31 a12
toBPV (ODD a1 a2 a3 _) = BPV a1 a2 a3 0 0 0
toBPV (TPV a23 a31 a12 _) = BPV 0 0 0 a23 a31 a12
toBPV (APS _ a1 a2 a3 a23 a31 a12 _) = BPV a1 a2 a3 a23 a31 a12

-- | 'toODD' takes any Cliffor and returns the ODD portion
toODD :: Cl3 -> Cl3
toODD R{} = ODD 0 0 0 0
toODD (V3 a1 a2 a3) = ODD a1 a2 a3 0
toODD BV{} = ODD 0 0 0 0
toODD (I a123) = ODD 0 0 0 a123
toODD (PV _ a1 a2 a3) = ODD a1 a2 a3 0
toODD H{} = ODD 0 0 0 0
toODD (C _ a123) = ODD 0 0 0 a123
toODD (BPV a1 a2 a3 _ _ _) = ODD a1 a2 a3 0
toODD (ODD a1 a2 a3 a123) = ODD a1 a2 a3 a123
toODD (TPV _ _ _ a123) = ODD 0 0 0 a123
toODD (APS _ a1 a2 a3 _ _ _ a123) = ODD a1 a2 a3 a123

-- | 'toTPV' takes any Cliffor and returns the TPV portion
toTPV :: Cl3 -> Cl3
toTPV R{} = TPV 0 0 0 0
toTPV V3{} = TPV 0 0 0 0
toTPV (BV a23 a31 a12) = TPV a23 a31 a12 0
toTPV (I a123) = TPV 0 0 0 a123
toTPV PV{} = TPV 0 0 0 0
toTPV (H _ a23 a31 a12) = TPV a23 a31 a12 0
toTPV (C _ a123) = TPV 0 0 0 a123
toTPV (BPV _ _ _ a23 a31 a12) = TPV a23 a31 a12 0
toTPV (ODD _ _ _ a123) = TPV 0 0 0 a123
toTPV (TPV a23 a31 a12 a123) = TPV a23 a31 a12 a123
toTPV (APS _ _ _ _ a23 a31 a12 a123) = TPV a23 a31 a12 a123

-- | 'toAPS' takes any Cliffor and returns the APS portion
toAPS :: Cl3 -> Cl3
toAPS (R a0) = APS a0 0 0 0 0 0 0 0
toAPS (V3 a1 a2 a3) = APS 0 a1 a2 a3 0 0 0 0
toAPS (BV a23 a31 a12) = APS 0 0 0 0 a23 a31 a12 0
toAPS (I a123) = APS 0 0 0 0 0 0 0 a123
toAPS (PV a0 a1 a2 a3) = APS a0 a1 a2 a3 0 0 0 0
toAPS (H a0 a23 a31 a12) = APS a0 0 0 0 a23 a31 a12 0
toAPS (C a0 a123) = APS a0 0 0 0 0 0 0 a123
toAPS (BPV a1 a2 a3 a23 a31 a12) = APS 0 a1 a2 a3 a23 a31 a12 0
toAPS (ODD a1 a2 a3 a123) = APS 0 a1 a2 a3 0 0 0 a123
toAPS (TPV a23 a31 a12 a123) = APS 0 0 0 0 a23 a31 a12 a123
toAPS (APS a0 a1 a2 a3 a23 a31 a12 a123) = APS a0 a1 a2 a3 a23 a31 a12 a123

-- derivatives of the functions in the Fractional Class for use in Jordan NF functon implemetnation
recip' :: Cl3 -> Cl3
recip' = negate.recip.(^2)   -- pole at 0

exp' :: Cl3 -> Cl3
exp' = exp

log' :: Cl3 -> Cl3
log' = recip  -- pole at 0

sqrt' :: Cl3 -> Cl3
sqrt' = (/2).recip.sqrt   -- pole at 0

sin' :: Cl3 -> Cl3
sin' = cos

cos' :: Cl3 -> Cl3
cos' = negate.sin

tan' :: Cl3 -> Cl3
tan' = recip.(^2).cos  -- pole at pi/2*n for all integers

asin' :: Cl3 -> Cl3
asin' = recip.sqrt.(1-).(^2)  -- pole at +/-1

acos' :: Cl3 -> Cl3
acos' = negate.recip.sqrt.(1-).(^2)  -- pole at +/-1

atan' :: Cl3 -> Cl3
atan' = recip.(1+).(^2)  -- pole at +/-i

sinh' :: Cl3 -> Cl3
sinh' = cosh

cosh' :: Cl3 -> Cl3
cosh' = sinh

tanh' :: Cl3 -> Cl3
tanh' = recip.(^2).cosh

asinh' :: Cl3 -> Cl3
asinh' = recip.sqrt.(1+).(^2)  -- pole at +/-i

acosh' :: Cl3 -> Cl3
acosh' x = recip $ sqrt (x - 1) * sqrt (x + 1)  -- pole at +/-1

atanh' :: Cl3 -> Cl3
atanh' = recip.(1-).(^2)  -- pole at +/-1


#ifndef O_NO_STORABLE
-------------------------------------------------------------------
-- 
-- Instance of Cl3 types with the "Foreign.Storable" library.
--  
-- For use with high performance data structures like Data.Vector.Storable
-- or Data.Array.Storable
-- 
-------------------------------------------------------------------

-- | Cl3 instance of Storable uses the APS constructor as its standard interface.
-- "peek" returns a cliffor constructed with APS. "poke" converts a cliffor to APS.
-- For a more compact storing of constructors other than APS use the storable
-- subtypes Cl3_R, Cl3_V3, Cl3_BV, Cl3_I, Cl3_PV, Cl3_H, Cl3_C, Cl3_BPV,
-- Cl3_ODD, Cl3_TPV.
instance Storable Cl3 where
  sizeOf _ = 8 * sizeOf (undefined :: Double)
  alignment _ = sizeOf (undefined :: Double)
  peek ptr = do
    a0 <- peek (offset 0)
    a1 <- peek (offset 1)
    a2 <- peek (offset 2)
    a3 <- peek (offset 3)
    a23 <- peek (offset 4)
    a31 <- peek (offset 5)
    a12 <- peek (offset 6)
    a123 <- peek (offset 7)
    return $ APS a0 a1 a2 a3 a23 a31 a12 a123
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)
  
  poke ptr (toAPS -> APS a0 a1 a2 a3 a23 a31 a12 a123) = do
    poke (offset 0) a0
    poke (offset 1) a1
    poke (offset 2) a2
    poke (offset 3) a3
    poke (offset 4) a23
    poke (offset 5) a31
    poke (offset 6) a12
    poke (offset 7) a123
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)
  poke _ _ = error "Serious Issues with poke in Cl3.Storable"


-- | 'Cl3_R' a compact storable data type for R.
data Cl3_R where
  Cl3_R :: !Double -> Cl3_R

-- | 'toCl3_R' converts a Cl3 value constructed with R to its compact form.
toCl3_R :: Cl3 -> Cl3_R
toCl3_R (R a0) = Cl3_R a0
toCl3_R err = error $ "Please don't try and cast something that's not R to Cl3_R, Got: " ++ show err

-- | 'fromCl3_R' converts the compact Cl3_R type back to a Cl3 type.
fromCl3_R :: Cl3_R -> Cl3
fromCl3_R (Cl3_R a0) = R a0

instance Show Cl3_R where
  show = show.fromCl3_R

#ifndef O_NO_DERIVED
instance Read Cl3_R where
  readPrec = toCl3_R <$> readPrec
#endif

instance Storable Cl3_R where
  sizeOf _ = sizeOf (undefined :: Double)
  alignment _ = sizeOf (undefined :: Double)
  peek ptr = do
    a0 <- peek offset
    return $ Cl3_R a0
      where
        offset = (castPtr ptr :: Ptr Double)

  poke ptr (Cl3_R a0) = do
    poke offset a0
      where
        offset = (castPtr ptr :: Ptr Double)


-- | 'Cl3_V3' a compact storable data type for V3.
data Cl3_V3 where
  Cl3_V3 :: !Double -> !Double -> !Double -> Cl3_V3

-- | 'toCl3_V3' converts a Cl3 value constructed with V3 to its compact form.
toCl3_V3 :: Cl3 -> Cl3_V3
toCl3_V3 (V3 a1 a2 a3) = Cl3_V3 a1 a2 a3
toCl3_V3 err = error $ "Please don't try and cast something that's not V3 to Cl3_V3, Got: " ++ show err

-- | 'fromCl3_V3' converts the compact Cl3_V3 type back to a Cl3 type.
fromCl3_V3 :: Cl3_V3 -> Cl3
fromCl3_V3 (Cl3_V3 a1 a2 a3) = V3 a1 a2 a3

instance Show Cl3_V3 where
  show = show.fromCl3_V3

#ifndef O_NO_DERIVED
instance Read Cl3_V3 where
  readPrec = toCl3_V3 <$> readPrec
#endif

instance Storable Cl3_V3 where
  sizeOf _ = 3 * sizeOf (undefined :: Double)
  alignment _ = sizeOf (undefined :: Double)
  peek ptr = do
    a1 <- peek (offset 0)
    a2 <- peek (offset 1)
    a3 <- peek (offset 2)
    return $ Cl3_V3 a1 a2 a3
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)

  poke ptr (Cl3_V3 a1 a2 a3) = do
    poke (offset 0) a1
    poke (offset 1) a2
    poke (offset 2) a3
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)


-- | 'Cl3_BV' a compact storable data type for BV.
data Cl3_BV where
  Cl3_BV :: !Double -> !Double -> !Double -> Cl3_BV

-- | 'toCl3_BV' converts a Cl3 value constructed with BV to its compact form.
toCl3_BV :: Cl3 -> Cl3_BV
toCl3_BV (BV a23 a31 a12) = Cl3_BV a23 a31 a12
toCl3_BV err = error $ "Please don't try and cast something that's not BV to Cl3_BV, Got: " ++ show err

-- | 'fromCl3_BV' converts the compact Cl3_BV type back to a Cl3 type.
fromCl3_BV :: Cl3_BV -> Cl3
fromCl3_BV (Cl3_BV a23 a31 a12) = BV a23 a31 a12

instance Show Cl3_BV where
  show = show.fromCl3_BV

#ifndef O_NO_DERIVED
instance Read Cl3_BV where
  readPrec = toCl3_BV <$> readPrec
#endif

instance Storable Cl3_BV where
  sizeOf _ = 3 * sizeOf (undefined :: Double)
  alignment _ = sizeOf (undefined :: Double)
  peek ptr = do
    a23 <- peek (offset 0)
    a31 <- peek (offset 1)
    a12 <- peek (offset 2)
    return $ Cl3_BV a23 a31 a12
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)

  poke ptr (Cl3_BV a23 a31 a12) = do
    poke (offset 0) a23
    poke (offset 1) a31
    poke (offset 2) a12
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)


-- | 'Cl3_I' a compact storable data type for I.
data Cl3_I where
  Cl3_I :: !Double -> Cl3_I

-- | 'toCl3_I' converts a Cl3 value constructed with I to its compact form.
toCl3_I :: Cl3 -> Cl3_I
toCl3_I (I a123) = Cl3_I a123
toCl3_I err = error $ "Please don't try and cast something that's not R to Cl3_R, Got: " ++ show err

-- | 'fromCl3_I' converts the compact Cl3_I type back to a Cl3 type.
fromCl3_I :: Cl3_I -> Cl3
fromCl3_I (Cl3_I a123) = I a123

instance Show Cl3_I where
  show = show.fromCl3_I

#ifndef O_NO_DERIVED
instance Read Cl3_I where
  readPrec = toCl3_I <$> readPrec
#endif

instance Storable Cl3_I where
  sizeOf _ = sizeOf (undefined :: Double)
  alignment _ = sizeOf (undefined :: Double)
  peek ptr = do
    a123 <- peek offset
    return $ Cl3_I a123
      where
        offset = (castPtr ptr :: Ptr Double)

  poke ptr (Cl3_I a123) = do
    poke offset a123
      where
        offset = (castPtr ptr :: Ptr Double)


-- | 'Cl3_PV' a compact storable data type for PV.
data Cl3_PV where
  Cl3_PV :: !Double -> !Double -> !Double -> !Double -> Cl3_PV

-- | 'toCl3_PV' converts a Cl3 value constructed with PV to its compact form.
toCl3_PV :: Cl3 -> Cl3_PV
toCl3_PV (PV a0 a1 a2 a3) = Cl3_PV a0 a1 a2 a3
toCl3_PV err = error $ "Please don't try and cast something that's not PV to Cl3_PV, Got: " ++ show err

-- | 'fromCl3_PV' converts the compact Cl3_PV type back to a Cl3 type.
fromCl3_PV :: Cl3_PV -> Cl3
fromCl3_PV (Cl3_PV a0 a1 a2 a3) = PV a0 a1 a2 a3

instance Show Cl3_PV where
  show = show.fromCl3_PV

#ifndef O_NO_DERIVED
instance Read Cl3_PV where
  readPrec = toCl3_PV <$> readPrec
#endif

instance Storable Cl3_PV where
  sizeOf _ = 4 * sizeOf (undefined :: Double)
  alignment _ = sizeOf (undefined :: Double)
  peek ptr = do
    a0 <- peek (offset 0)
    a1 <- peek (offset 1)
    a2 <- peek (offset 2)
    a3 <- peek (offset 4)
    return $ Cl3_PV a0 a1 a2 a3
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)

  poke ptr (Cl3_PV a0 a1 a2 a3) = do
    poke (offset 0) a0
    poke (offset 1) a1
    poke (offset 2) a2
    poke (offset 3) a3
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)


-- | 'Cl3_H' a compact storable data type for H.
data Cl3_H where
  Cl3_H :: !Double -> !Double -> !Double -> !Double -> Cl3_H

-- | 'toCl3_H' converts a Cl3 value constructed with H to its compact form.
toCl3_H :: Cl3 -> Cl3_H
toCl3_H (H a0 a23 a31 a12) = Cl3_H a0 a23 a31 a12
toCl3_H err = error $ "Please don't try and cast something that's not H to Cl3_H, Got: " ++ show err

-- | 'fromCl3_H' converts the compact Cl3_H type back to a Cl3 type.
fromCl3_H :: Cl3_H -> Cl3
fromCl3_H (Cl3_H a0 a23 a31 a12) = H a0 a23 a31 a12

instance Show Cl3_H where
  show = show.fromCl3_H

#ifndef O_NO_DERIVED
instance Read Cl3_H where
  readPrec = toCl3_H <$> readPrec
#endif

instance Storable Cl3_H where
  sizeOf _ = 4 * sizeOf (undefined :: Double)
  alignment _ = sizeOf (undefined :: Double)
  peek ptr = do
    a0 <- peek (offset 0)
    a23 <- peek (offset 1)
    a31 <- peek (offset 2)
    a12 <- peek (offset 3)
    return $ Cl3_H a0 a23 a31 a12
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)

  poke ptr (Cl3_H a0 a23 a31 a12) = do
    poke (offset 0) a0
    poke (offset 1) a23
    poke (offset 2) a31
    poke (offset 3) a12
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)


-- | 'Cl3_C' a compact storable data type for C.
data Cl3_C where
  Cl3_C :: !Double -> !Double -> Cl3_C

-- | 'toCl3_C' converts a Cl3 value constructed with C to its compact form.
toCl3_C :: Cl3 -> Cl3_C
toCl3_C (C a0 a123) = Cl3_C a0 a123
toCl3_C err = error $ "Please don't try and cast something that's not C to Cl3_C, Got: " ++ show err

-- | 'fromCl3_C' converts the compact Cl3_C type back to a Cl3 type.
fromCl3_C :: Cl3_C -> Cl3
fromCl3_C (Cl3_C a0 a123) = C a0 a123

instance Show Cl3_C where
  show = show.fromCl3_C

#ifndef O_NO_DERIVED
instance Read Cl3_C where
  readPrec = toCl3_C <$> readPrec
#endif

instance Storable Cl3_C where
  sizeOf _ = 2 * sizeOf (undefined :: Double)
  alignment _ = sizeOf (undefined :: Double)
  peek ptr = do
    a0 <- peek (offset 0)
    a123 <- peek (offset 1)
    return $ Cl3_C a0 a123
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)

  poke ptr (Cl3_C a0 a123) = do
    poke (offset 0) a0
    poke (offset 1) a123
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)


-- | 'Cl3_BPV' a compact storable data type for BPV.
data Cl3_BPV where
  Cl3_BPV :: !Double -> !Double -> !Double -> !Double -> !Double -> !Double -> Cl3_BPV

-- | 'toCl3_BPV' converts a Cl3 value constructed with BPV to its compact form.
toCl3_BPV :: Cl3 -> Cl3_BPV
toCl3_BPV (BPV a1 a2 a3 a23 a31 a12) = Cl3_BPV a1 a2 a3 a23 a31 a12
toCl3_BPV err = error $ "Please don't try and cast something that's not BPV to Cl3_BPV, Got: " ++ show err

-- | 'fromCl3_BPV' converts the compact Cl3_BPV type back to a Cl3 type.
fromCl3_BPV :: Cl3_BPV -> Cl3
fromCl3_BPV (Cl3_BPV a1 a2 a3 a23 a31 a12) = BPV a1 a2 a3 a23 a31 a12

instance Show Cl3_BPV where
  show = show.fromCl3_BPV

#ifndef O_NO_DERIVED
instance Read Cl3_BPV where
  readPrec = toCl3_BPV <$> readPrec
#endif

instance Storable Cl3_BPV where
  sizeOf _ = 6 * sizeOf (undefined :: Double)
  alignment _ = sizeOf (undefined :: Double)
  peek ptr = do
    a1 <- peek (offset 0)
    a2 <- peek (offset 1)
    a3 <- peek (offset 2)
    a23 <- peek (offset 3)
    a31 <- peek (offset 4)
    a12 <- peek (offset 5)
    return $ Cl3_BPV a1 a2 a3 a23 a31 a12
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)

  poke ptr (Cl3_BPV a1 a2 a3 a23 a31 a12) = do
    poke (offset 0) a1
    poke (offset 1) a2
    poke (offset 2) a3
    poke (offset 3) a23
    poke (offset 4) a31
    poke (offset 5) a12
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)


-- | 'Cl3_ODD' a compact storable data type for ODD.
data Cl3_ODD where
  Cl3_ODD :: !Double -> !Double -> !Double -> !Double -> Cl3_ODD

-- | 'toCl3_ODD' converts a Cl3 value constructed with ODD to its compact form.
toCl3_ODD :: Cl3 -> Cl3_ODD
toCl3_ODD (ODD a1 a2 a3 a123) = Cl3_ODD a1 a2 a3 a123
toCl3_ODD err = error $ "Please don't try and cast something that's not ODD to Cl3_ODD, Got: " ++ show err

-- | 'fromCl3_ODD' converts the compact Cl3_ODD type back to a Cl3 type.
fromCl3_ODD :: Cl3_ODD -> Cl3
fromCl3_ODD (Cl3_ODD a1 a2 a3 a123) = ODD a1 a2 a3 a123

instance Show Cl3_ODD where
  show = show.fromCl3_ODD

#ifndef O_NO_DERIVED
instance Read Cl3_ODD where
  readPrec = toCl3_ODD <$> readPrec
#endif

instance Storable Cl3_ODD where
  sizeOf _ = 4 * sizeOf (undefined :: Double)
  alignment _ = sizeOf (undefined :: Double)
  peek ptr = do
    a1 <- peek (offset 0)
    a2 <- peek (offset 1)
    a3 <- peek (offset 2)
    a123 <- peek (offset 3)
    return $ Cl3_ODD a1 a2 a3 a123
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)

  poke ptr (Cl3_ODD a1 a2 a3 a123) = do
    poke (offset 0) a1
    poke (offset 1) a2
    poke (offset 2) a3
    poke (offset 3) a123
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)


-- | 'Cl3_TPV' a compact storable data type for TPV.
data Cl3_TPV where
  Cl3_TPV :: !Double -> !Double -> !Double -> !Double -> Cl3_TPV

-- | 'toCl3_TPV' converts a Cl3 value constructed with TPV to its compact form.
toCl3_TPV :: Cl3 -> Cl3_TPV
toCl3_TPV (TPV a23 a31 a12 a123) = Cl3_TPV a23 a31 a12 a123
toCl3_TPV err = error $ "Please don't try and cast something that's not TPV to Cl3_TPV, Got: " ++ show err

-- | 'fromCl3_TPV' converts the compact Cl3_TPV type back to a Cl3 type.
fromCl3_TPV :: Cl3_TPV -> Cl3
fromCl3_TPV (Cl3_TPV a23 a31 a12 a123) = TPV a23 a31 a12 a123

instance Show Cl3_TPV where
  show = show.fromCl3_TPV

#ifndef O_NO_DERIVED
instance Read Cl3_TPV where
  readPrec = toCl3_TPV <$> readPrec
#endif

instance Storable Cl3_TPV where
  sizeOf _ = 4 * sizeOf (undefined :: Double)
  alignment _ = sizeOf (undefined :: Double)
  peek ptr = do
    a23 <- peek (offset 0)
    a31 <- peek (offset 1)
    a12 <- peek (offset 2)
    a123 <- peek (offset 3)
    return $ Cl3_TPV a23 a31 a12 a123
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)

  poke ptr (Cl3_TPV a23 a31 a12 a123) = do
    poke (offset 0) a23
    poke (offset 1) a31
    poke (offset 2) a12
    poke (offset 3) a123
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)


-- | 'Cl3_APS' a compact storable data type for APS.
data Cl3_APS where
  Cl3_APS :: !Double -> !Double -> !Double -> !Double -> !Double -> !Double -> !Double -> !Double -> Cl3_APS

-- | 'toCl3_APS' converts a Cl3 value constructed with APS to its compact form.
toCl3_APS :: Cl3 -> Cl3_APS
toCl3_APS (APS a0 a1 a2 a3 a23 a31 a12 a123) = Cl3_APS a0 a1 a2 a3 a23 a31 a12 a123
toCl3_APS err = error $ "Please don't try and cast something that's not APS to Cl3_APS, Got: " ++ show err

-- | 'fromCl3_APS' converts the compact Cl3_APS type back to a Cl3 type.
fromCl3_APS :: Cl3_APS -> Cl3
fromCl3_APS (Cl3_APS a0 a1 a2 a3 a23 a31 a12 a123) = APS a0 a1 a2 a3 a23 a31 a12 a123

instance Show Cl3_APS where
  show = show.fromCl3_APS

#ifndef O_NO_DERIVED
instance Read Cl3_APS where
  readPrec = toCl3_APS <$> readPrec
#endif

instance Storable Cl3_APS where
  sizeOf _ = 8 * sizeOf (undefined :: Double)
  alignment _ = sizeOf (undefined :: Double)
  peek ptr = do
    a0 <- peek (offset 0)
    a1 <- peek (offset 1)
    a2 <- peek (offset 2)
    a3 <- peek (offset 3)
    a23 <- peek (offset 4)
    a31 <- peek (offset 5)
    a12 <- peek (offset 6)
    a123 <- peek (offset 7)
    return $ Cl3_APS a0 a1 a2 a3 a23 a31 a12 a123
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)

  poke ptr (Cl3_APS a0 a1 a2 a3 a23 a31 a12 a123) = do
    poke (offset 0) a0
    poke (offset 1) a1
    poke (offset 2) a2
    poke (offset 3) a3
    poke (offset 4) a23
    poke (offset 5) a31
    poke (offset 6) a12
    poke (offset 7) a123
      where
        offset i = (castPtr ptr :: Ptr Double) `plusPtr` (i*8)


#endif



#ifndef O_NO_RANDOM
-------------------------------------------------------------------
-- 
-- Random Instance of Cl3 types with the "System.Random" library.
-- 
--
-- Random helper functions will be based on the "abs x * signum x" decomposition
-- for the single grade elements. The "abs x" will be the random magnitude that
-- is by the default [0,1), and the "signum x" part will be a random direction
-- of a vector or the sign of a scalar. The multi-grade elements will be constructed from
-- a combination of the single grade generators.  Each grade will be evenly
-- distributed across the range.
-- 
-------------------------------------------------------------------

-- | 'Random' instance for the 'System.Random' library
instance Random Cl3 where
  randomR (minAbs,maxAbs) g =
    case randomR (fromEnum (minBound :: ConCl3), fromEnum (maxBound :: ConCl3)) g of
      (r, g') -> case toEnum r of
        ConR -> rangeR (minAbs,maxAbs) g'
        ConV3 -> rangeV3 (minAbs,maxAbs) g'
        ConBV -> rangeBV (minAbs,maxAbs) g'
        ConI -> rangeI (minAbs,maxAbs) g'
        ConPV -> rangePV (minAbs,maxAbs) g'
        ConH -> rangeH (minAbs,maxAbs) g'
        ConC -> rangeC (minAbs,maxAbs) g'
        ConBPV -> rangeBPV (minAbs,maxAbs) g'
        ConODD -> rangeODD (minAbs,maxAbs) g'
        ConTPV -> rangeTPV (minAbs,maxAbs) g'
        ConAPS -> rangeAPS (minAbs,maxAbs) g'
        ConProj -> rangeProjector (minAbs,maxAbs) g'
        ConNilpotent -> rangeNilpotent (minAbs,maxAbs) g'
        ConUnitary -> rangeUnitary (minAbs,maxAbs) g'

  random = randomR (0,1)



-- | 'ConCl3' Bounded Enum Algebraic Data Type of constructors of Cl3
data ConCl3 = ConR
            | ConV3
            | ConBV
            | ConI
            | ConPV
            | ConH
            | ConC
            | ConBPV
            | ConODD
            | ConTPV
            | ConAPS
            | ConProj
            | ConNilpotent
            | ConUnitary
  deriving (Bounded, Enum)




-- | 'randR' random Real Scalar (Grade 0) with random magnitude and random sign
randR :: RandomGen g => g -> (Cl3, g)
randR = rangeR (0,1)


-- | 'rangeR' random Real Scalar (Grade 0) with random magnitude within a range and a random sign
rangeR :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeR = scalarHelper R


-- | 'randV3' random Vector (Grade 1) with random magnitude and random direction
-- the direction is using spherical coordinates
randV3 :: RandomGen g => g -> (Cl3, g)
randV3 = rangeV3 (0,1)


-- | 'rangeV3' random Vector (Grade 1) with random magnitude within a range and a random direction
rangeV3 :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeV3 = vectorHelper V3


-- | 'randBV' random Bivector (Grade 2) with random magnitude and random direction
-- the direction is using spherical coordinates
randBV :: RandomGen g => g -> (Cl3, g)
randBV = rangeBV (0,1)


-- | 'rangeBV' random Bivector (Grade 2) with random magnitude in a range and a random direction
rangeBV :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeBV = vectorHelper BV


-- | 'randI' random Imaginary Scalar (Grade 3) with random magnitude and random sign
randI :: RandomGen g => g -> (Cl3, g)
randI = rangeI (0,1)


-- | 'rangeI' random Imaginary Scalar (Grade 3) with random magnitude within a range and random sign
rangeI :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeI = scalarHelper I


-- | 'randPV' random Paravector made from random Grade 0 and Grade 1 elements
randPV :: RandomGen g => g -> (Cl3, g)
randPV = rangePV (0,1)


-- | 'rangePV' random Paravector made from random Grade 0 and Grade 1 elements within a range
rangePV :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangePV (lo, hi) g =
  let (R scale, g') = rangeR (lo, hi) g
      (R a0, g'') = randR g'
      (V3 a1 a2 a3, g''') = randV3 g''
      sumsqs = a1^2 + a2^2 + a3^2
      x = abs a0 * sqrt sumsqs
      invMag = recip.sqrt $ a0^2 + sumsqs + x + x
      mag = scale * invMag
  in (PV (mag * a0) (mag * a1) (mag * a2) (mag * a3), g''')


-- | 'randH' random Quaternion made from random Grade 0 and Grade 2 elements
randH :: RandomGen g => g -> (Cl3, g)
randH = rangeH (0,1)


-- | 'rangeH' random Quaternion made from random Grade 0 and Grade 2 elements within a range
rangeH :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeH (lo, hi) g =
  let (R scale, g') = rangeR (lo, hi) g
      (R a0, g'') = randR g'
      (BV a23 a31 a12, g''') = randBV g''
      invMag = recip.sqrt $ a0^2 + a23^2 + a31^2 + a12^2
      mag = scale * invMag
  in (H (mag * a0) (mag * a23) (mag * a31) (mag * a12), g''')


-- | 'randC' random combination of Grade 0 and Grade 3
randC :: RandomGen g => g -> (Cl3, g)
randC = rangeC (0,1)


-- | 'rangeC' random combination of Grade 0 and Grade 3 within a range
rangeC :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeC (lo, hi) g =
  let (R scale, g') = rangeR (lo, hi) g
      (phi, g'') = randomR (0, 2*pi) g'
  in (C (scale * cos phi) (scale * sin phi), g'')


-- | 'randBPV' random combination of Grade 1 and Grade 2
randBPV :: RandomGen g => g -> (Cl3, g)
randBPV = rangeBPV (0,1)


-- | 'rangeBPV' random combination of Grade 1 and Grade 2 within a range
rangeBPV :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeBPV (lo, hi) g =
  let (R scale, g') = rangeR (lo, hi) g
      (V3 a1 a2 a3, g'') = randV3 g'
      (BV a23 a31 a12, g''') = randBV g''
      x = sqrt $ (a1*a31 - a2*a23)^2 + (a1*a12 - a3*a23)^2 + (a2*a12 - a3*a31)^2
      invMag = recip.sqrt $ a1^2 + a23^2 + a2^2 + a31^2 + a3^2 + a12^2 + x + x
      mag = scale * invMag
  in (BPV (mag * a1) (mag * a2) (mag * a3) (mag * a23) (mag * a31) (mag * a12), g''')


-- | 'randODD' random combination of Grade 1 and Grade 3
randODD :: RandomGen g => g -> (Cl3, g)
randODD = rangeODD (0,1)


-- | 'rangeODD' random combination of Grade 1 and Grade 3 within a range
rangeODD :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeODD (lo, hi) g =
  let (R scale, g') = rangeR (lo, hi) g
      (V3 a1 a2 a3, g'') = randV3 g'
      (I a123, g''') = randI g''
      invMag = recip.sqrt $ a1^2 + a2^2 + a3^2 + a123^2
      mag = scale * invMag
  in (ODD (mag * a1) (mag * a2) (mag * a3) (mag * a123), g''')


-- | 'randTPV' random combination of Grade 2 and Grade 3
randTPV :: RandomGen g => g -> (Cl3, g)
randTPV = rangeTPV (0,1)


-- | 'rangeTPV' random combination of Grade 2 and Grade 3 within a range
rangeTPV :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeTPV (lo, hi) g =
  let (R scale, g') = rangeR (lo, hi) g
      (BV a23 a31 a12, g'') = randBV g'
      (I a123, g''') = randI g''
      sumsqs = a23^2 + a31^2 + a12^2
      x = abs a123 * sqrt sumsqs
      invMag = recip.sqrt $ sumsqs + a123^2 + x + x
      mag = scale * invMag
  in (TPV (mag * a23) (mag * a31) (mag * a12) (mag * a123), g''')


-- | 'randAPS' random combination of all 4 grades
randAPS :: RandomGen g => g -> (Cl3, g)
randAPS = rangeAPS (0,1)


-- | 'rangeAPS' random combination of all 4 grades within a range
rangeAPS :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeAPS (lo, hi) g =
  let (R scale, g') = rangeR (lo, hi) g
      (C a0 a123, g'') = randC g'
      (V3 a1 a2 a3, g''') = randV3 g''
      (BV a23 a31 a12, g'v) = randBV g'''
      x = sqrt $ (a0*a1 + a123*a23)^2 + (a0*a2 + a123*a31)^2 + (a0*a3 + a123*a12)^2 + (a2*a12 - a3*a31)^2 + (a3*a23 - a1*a12)^2 + (a1*a31 - a2*a23)^2
      invMag = recip.sqrt $ a0^2 + a1^2 + a2^2 + a3^2 + a23^2 + a31^2 + a12^2 + a123^2 + x + x
      mag = scale * invMag
  in (APS (mag * a0) (mag * a1) (mag * a2) (mag * a3) (mag * a23) (mag * a31) (mag * a12) (mag * a123), g'v)


-------------------------------------------------------------------
-- Additional Random generators
-------------------------------------------------------------------
-- | 'randUnitV3' a unit vector with a random direction
randUnitV3 :: RandomGen g => g -> (Cl3, g)
randUnitV3 g =
  let (theta, g') = randomR (0,2*pi) g
      (u, g'') = randomR (-1,1) g'
      semicircle = sqrt (1-u^2)
  in (V3 (semicircle * cos theta) (semicircle * sin theta) u, g'')


-- | 'randProjector' a projector with a random direction
randProjector :: RandomGen g => g -> (Cl3, g)
randProjector g =
  let (V3 a1 a2 a3, g') = randUnitV3 g
  in (PV 0.5 (0.5 * a1) (0.5 * a2) (0.5 * a3), g')


-- | 'rangeProjector' a projector with a range of random magnitudes and directions
rangeProjector :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeProjector (lo, hi) g =
  let (R mag, g') = rangeR (lo, hi) g
      (PV a0 a1 a2 a3, g'') = randProjector g'
  in (PV (mag * a0) (mag * a1) (mag * a2) (mag * a3), g'')


-- | 'randNilpotent' a nilpotent element with a random orientation
randNilpotent :: RandomGen g => g -> (Cl3, g)
randNilpotent g =
  let (PV a0 a1 a2 a3, g') = randProjector g
      (V3 b1 b2 b3, g'') = randUnitV3 g'
      c1 = a2*b3 - a3*b2
      c2 = a3*b1 - a1*b3
      c3 = a1*b2 - a2*b1 -- (V3 c1 c2 c3) vector normal to the projector: mIx.toBV $ toV3 p * v
      invMag = recip.sqrt $ c1^2 + c2^2 + c3^2
      d1 = invMag * c1
      d2 = invMag * c2
      d3 = invMag * c3  -- (V3 d1 d2 d3) unit vector normal to the projector
  in (BPV (d1*a0) (d2*a0) (d3*a0) (d2*a3 - d3*a2) (d3*a1 - d1*a3) (d1*a2 - d2*a1), g'')


-- | 'rangeNilpotent' a nilpotent with a range of random magnitudes and orientations
rangeNilpotent :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeNilpotent (lo, hi)  g =
  let (R mag, g') = rangeR (lo, hi) g
      (BPV a1 a2 a3 a23 a31 a12, g'') = randNilpotent g'
  in (BPV (mag * a1) (mag * a2) (mag * a3) (mag * a23) (mag * a31) (mag * a12), g'')


-- | 'randUnitary' a unitary element with a random orientation
randUnitary :: RandomGen g => g -> (Cl3, g)
randUnitary g =
  let (tpv,g') = randTPV g
  in (exp tpv,g')


-- | 'rangeUnitary' a unitary element with a range of random magnitudes and orientations, the exponential of a triparavector
rangeUnitary :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeUnitary (lo, hi) g =
  let (tpv, g') = rangeTPV (lo, hi) g
  in (exp tpv, g')


-------------------------------------------------------------------
-- helper functions
-------------------------------------------------------------------
magHelper :: RandomGen g => (Cl3, Cl3) -> g -> (Double, g)
magHelper (lo, hi) g =
  let R lo' = abs lo
      R hi' = abs hi
  in randomR (lo', hi') g


scalarHelper :: RandomGen g => (Double -> Cl3) -> (Cl3, Cl3) -> g -> (Cl3, g)
scalarHelper con rng g =
  let (mag, g') = magHelper rng g
      (sign, g'') = random g'
  in if sign
     then (con mag, g'')
     else (con (negate mag), g'')


vectorHelper :: RandomGen g => (Double -> Double -> Double -> Cl3) -> (Cl3, Cl3) -> g -> (Cl3, g)
vectorHelper con rng g =
  let (mag, g') = magHelper rng g
      (V3 x y z, g'') = randUnitV3 g'
  in (con (mag * x) (mag * y) (mag * z), g'')


#endif

-- End of File
