{-# LANGUAGE Safe #-}

-------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2017 Nathan Waivio
-- License     :  BSD3
-- Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
-- Stability   :  Stable
-- Portability :  unportable
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


module Algebra.Geometric.Cl3.Random
(
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
 randNilpotent
) where

import Algebra.Geometric.Cl3 (Cl3(..),toV3,toBV,toBPV)
import System.Random (RandomGen, Random, randomR, random)


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
  let (r, g') = rangeR (lo, hi) g
      (v3, g'') = rangeV3 (lo, hi) g'
  in (r + v3, g'')


-- | 'randH' random Quarternion made from random Grade 0 and Grade 2 elements
randH :: RandomGen g => g -> (Cl3, g)
randH = rangeH (0,1)


-- | 'rangeH' random Quarternion made from random Grade 0 and Grade 2 elements within a range
rangeH :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeH (lo, hi) g =
  let (r, g') = rangeR (lo, hi) g
      (bv, g'') = rangeBV (lo, hi) g'
  in (r + bv, g'')


-- | 'randC' random combination of Grade 0 and Grade 3
randC :: RandomGen g => g -> (Cl3, g)
randC = rangeC (0,1)


-- | 'rangeC' random combination of Grade 0 and Grade 3 within a range
rangeC :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeC (lo, hi) g =
  let (r, g') = rangeR (lo, hi) g
      (i, g'') = rangeI (lo, hi) g'
  in (r + i, g'')


-- | 'randBPV' random combination of Grade 1 and Grade 2
randBPV :: RandomGen g => g -> (Cl3, g)
randBPV = rangeBPV (0,1)


-- | 'rangeBPV' random combination of Grade 1 and Grade 2 within a range
rangeBPV :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeBPV (lo, hi) g =
  let (v3, g') = rangeV3 (lo, hi) g
      (bv, g'') = rangeBV (lo, hi) g'
  in (v3 + bv, g'')


-- | 'randODD' random combination of Grade 1 and Grade 3
randODD :: RandomGen g => g -> (Cl3, g)
randODD = rangeODD (0,1)


-- | 'rangeODD' random combination of Grade 1 and Grade 3 within a range
rangeODD :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeODD (lo, hi) g =
  let (v3, g') = rangeV3 (lo, hi) g
      (i, g'') = rangeI (lo, hi) g'
  in (v3 + i, g'')


-- | 'randTPV' random combination of Grade 2 and Grade 3
randTPV :: RandomGen g => g -> (Cl3, g)
randTPV = rangeTPV (0,1)


-- | 'rangeTPV' random combination of Grade 2 and Grade 3 within a range
rangeTPV :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeTPV (lo, hi) g =
  let (bv, g') = rangeBV (lo, hi) g
      (i, g'') = rangeI (lo, hi) g'
  in (bv + i, g'')


-- | 'randAPS' random combination of all 4 grades
randAPS :: RandomGen g => g -> (Cl3, g)
randAPS = rangeAPS (0,1)


-- | 'rangeAPS' random combination of all 4 grades within a range
rangeAPS :: RandomGen g => (Cl3, Cl3) -> g -> (Cl3, g)
rangeAPS (lo, hi) g =
  let (pv, g') = rangePV (lo, hi) g
      (tpv, g'') = rangeTPV (lo, hi) g'
  in (pv + tpv, g'')


-------------------------------------------------------------------
-- Additional Random generators
-------------------------------------------------------------------
-- | 'randUnitV3' a unit vector with a random direction
randUnitV3 :: RandomGen g => g -> (Cl3, g)
randUnitV3 g = 
  let (theta, g') = randomR (0,pi) g
      (phi, g'') = randomR (0,2*pi) g'
  in (V3 (sin theta * cos phi) (sin theta * sin phi) (cos theta), g'')


-- | 'randProjector' a projector with a random direction
randProjector :: RandomGen g => g -> (Cl3, g)
randProjector g =
  let (v3, g') = randUnitV3 g
  in (0.5 + 0.5 * v3, g')


-- | 'randNilpotent' a nilpotent element with a random orientation
randNilpotent :: RandomGen g => g -> (Cl3, g)
randNilpotent g =
  let (p, g') = randProjector g
      (v, g'') = randUnitV3 g'
      vnormal = signum $ I (-1) * toBV ( toV3 p * v)  -- unit vector normal to the projector
  in (toBPV $ vnormal * p, g'')


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
      (theta, g'') = randomR (0,pi) g'
      (phi, g''') = randomR (0,2*pi) g''
  in (con (mag * sin theta * cos phi) (mag * sin theta * sin phi) (mag * cos theta), g''')

