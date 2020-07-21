{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ == 810
-- Work around to fix GHC Issue #15304, issue popped up again in GHC 8.10, it should be fixed in GHC 8.12
-- This code is meant to reproduce MR 2608 for GHC 8.10
{-# OPTIONS_GHC -funfolding-keeness-factor=1 -funfolding-use-threshold=80 #-}
#endif

-------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2017-2020 Nathan Waivio
-- License     :  BSD3
-- Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
-- 
-- A program to test Algebra.Geometric.Cl3
-- The code runs tests on some standard test input and then
-- runs quckcheck for some trig identities.
-- 
-------------------------------------------------------------------

module Main (main) where

import Algebra.Geometric.Cl3
import Control.Monad (replicateM)
import Criterion.Main (defaultMain, bench, nfIO, env, Benchmark)
import System.Random (randomRIO)

------------------------------------------------------------------
-- |
-- This program verifies the approximate equality of various trig
-- identities to the with the following limitations:
-- 
-- * The magnitude of the cliffor is limited in some cases.
--
-- * The imaginary part of the eigenvalues are unwrapped, due to the cyclical nature of some of the results, in a few cases.
--
-- * The poles of the functions are excluded.
--
-- * The poles of the derivatives of the functions are excluded when the cliffor is has a nilpotent component.
--
-- * Approximate equivalence is tested due to limitations with respect to floating point math.
--
-- 
-- The following properties are verified in this module:
--
-- * log.exp Identity
--
-- * exp.log Identity
--
-- * abs*signum law
--
-- * The definition of recip
--
-- * recip.recip Identity
--
-- * sin.asin Identity
--
-- * asin.sin Identity
--
-- * cos.acos Identity
--
-- * acos.cos Identity
--
-- * sinh.asinh Identity
--
-- * asinh.sinh Identity
--
-- * cosh.acosh Identity
--
-- * acosh.cosh Identity
--
-- * Double Sin Identity
--
-- * Double Cos Identity
--
-- * Double Tan Identity
--
-- * Double Sinh Identity
--
-- * Double Cosh Identity
--
-- * Double Tanh Identity
--
-- * Positive Sin Shift Identity
--
-- * Negative Sin Shift Identity
--
-- * sin^2+cos^2 Identity
--
-- * cosh^2-sinh^2 Identity
--
-- * Symmetry of Cosh
--
-- * Symmetry of Sinh
--
-- * Double I Sin
--
-- * Composition Algebra Tests
--
-------------------------------------------------------------------


main :: IO ()
main = defaultMain benchList

benchList :: [Benchmark]
benchList = fmap buildBench props

props :: [(String,(Cl3 -> Bool))]
props = [("Testing log.exp Identity:", prop_LogExp),
         ("Testing exp.log Identity:", prop_ExpLog),
         ("Testing abs*signum law:", prop_AbsSignum),
         ("Testing the definition of recip:", prop_RecipDef),
         ("Testing recip.recip Identity:", prop_RecipID),
         ("Testing sin.asin Identity:", prop_SinAsin),
         ("Testing asin.sin Identity:", prop_AsinSin),
         ("Testing cos.acos Identity:", prop_CosAcos),
         ("Testing acos.cos Identity:", prop_AcosCos),
         ("Testing sinh.asinh Identity:", prop_SinhAsinh),
         ("Testing asinh.sinh Identity:", prop_AsinhSinh),
         ("Testing cosh.acosh Identity:", prop_CoshAcosh),
         ("Testing acosh.cosh Identity:", prop_AcoshCosh),
         ("Testing acosh.cosh Identity2:", prop_AcoshCosh2),
         ("Testing Double Sin Identity:", prop_DubSin),
         ("Testing Double Cos Identity:", prop_DubCos),
         ("Testing Double Tan Identity:", prop_DubTan),
         ("Testing Double Sinh Identity:", prop_DubSinh),
         ("Testing Double Cosh Identity:", prop_DubCosh),
         ("Testing Double Tanh Identity:", prop_DubTanh),
         ("Testing Positive Sin Shift Identity:", prop_PosSinShift),
         ("Testing Negative Sin Shift Identity:", prop_NegSinShift),
         ("Testing sin^2+cos^2 Identity:", prop_SinSqCosSq),
         ("Testing cosh^2-sinh^2 Identity:", prop_CoshSqmSinhSq),
         ("Testing Symmetry of Cosh:", prop_SymCosh),
         ("Testing Symmetry of Sinh:", prop_SymSinh),
         ("Testing Double I Sin:", prop_DoubleISin)]

buildBench :: (String,(Cl3 -> Bool)) -> Benchmark
buildBench (name, prop) = runWithEnv $ \cliffs -> bench name (nfIO $ test cliffs)
  where
    test :: [Cl3] -> IO ()
    test ([]) = return ()
    test (cl:cls) =
      if prop cl
      then test cls
      else error $ "Failed on input: " ++ show cl

runWithEnv :: ([Cl3] -> Benchmark) -> Benchmark
runWithEnv = (env listRandCliffs)

listRandCliffs :: IO [Cl3]
listRandCliffs = do
  randCliff <-(replicateM 5000000).randomRIO $ (R 0, R 3)
  return (inputs ++ randCliff)

-- Standard inputs and special cases of projectors and nilpotents
inputs :: [Cl3]
inputs = [R 0
         ,APS 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8
         ,PV 0.5 0 0 0.5
         ,PV 0.5 0 0 (-0.5)
         ,BPV 0.5 0 0 0 (-0.5) 0
         ,BPV 0.5 0 0 0 0.5 0
         ,R 1
         ,R (-1)
         ,R pi
         ,R (pi/2)
         ,R (pi/4)
         ,V3 1 0 0
         ,APS 1 0.5 0 0 0 0.5 0 0
         ,APS 1 0.5 0 0 0 (-0.5) 0 0
         ,PV 1 1 0 0
         ,V3 1 0 0
         ,V3 (-1) 0 0
         ,V3 0 1 0
         ,V3 0 (-1) 0
         ,V3 0 0 1
         ,V3 0 0 (-1)
         ,V3 pi 0 0
         ,V3 (pi/2) 0 0
         ,V3 (pi/4) 0 0
         ,BV 1 0 0
         ,BV (-1) 0 0
         ,BV 0 1 0
         ,BV 0 (-1) 0
         ,BV 0 0 1
         ,BV 0 0 (-1)
         ,BV pi 0 0
         ,BV (pi/2) 0 0
         ,BV (pi/4) 0 0
         ,I 1
         ,I (-1)
         ,I pi
         ,I (pi/2)
         ,I (pi/4)
         ]


-------------------------------------------------------
-- | A set of properties to test
-------------------------------------------------------

prop_LogExp :: Cl3 -> Bool
prop_LogExp (cliffor) = (abs cliffor > 10) || (
  let cliffor' = unWrapIPartEigs cliffor  -- imaginary part of log.exp repeats
-- round off errors get large for exp larger than 5 use spectproj (log.exp) for accuracy
-- note: +/- i*pi are not really poles but cause issues due to cancelation for (BV pi 0 0)
  in poles [I (-pi), I (pi)] cliffor' || (log (exp cliffor') ≈≈ cliffor'))

-- log 0 is -Inf, Infinite vectors don't play nice
-- spectproj (exp.log) doesn't have this issue
prop_ExpLog :: Cl3 -> Bool
prop_ExpLog (cliffor) = (lsv cliffor < tol) || (exp (log cliffor) ≈≈ cliffor)

prop_AbsSignum :: Cl3 -> Bool
prop_AbsSignum (cliffor) = abs cliffor * signum cliffor ≈≈ cliffor

prop_RecipDef :: Cl3 -> Bool
prop_RecipDef (cliffor) = (lsv cliffor < tol) || (recip cliffor * cliffor ≈≈ 1)

-- singular inputs don't recip also suffers from roundoff errors at large values
prop_RecipID :: Cl3 -> Bool
prop_RecipID (cliffor) = (lsv cliffor < tol) || (recip (recip cliffor) ≈≈ cliffor)

prop_SinAsin :: Cl3 -> Bool
prop_SinAsin (cliffor) = if hasNilpotent cliffor
                         then poles [R 1, R (-1)] cliffor || (sin (asin cliffor) ≈≈ cliffor)
                         else sin (asin cliffor) ≈≈ cliffor

prop_AsinSin :: Cl3 -> Bool
prop_AsinSin (cliffor) = (abs cliffor > 10) || (asin (sin cliffor) ≈≈ (I (-1) * log (0.5 * (exp (I 1 * cliffor) - exp (mIx cliffor)) +
                                                                                     sqrt (1+0.25*(exp (mIx cliffor) - exp (I 1 * cliffor))^2))))

prop_CosAcos :: Cl3 -> Bool
prop_CosAcos (cliffor) = if hasNilpotent cliffor
                             then poles [R 1, R (-1)] cliffor || (cos (acos cliffor) ≈≈ cliffor)
                             else cos (acos cliffor) ≈≈ cliffor

prop_AcosCos :: Cl3 -> Bool
prop_AcosCos (cliffor) = (abs cliffor > 10) || (if hasNilpotent cliffor
                                                then poles [R 0, pi, negate pi] cliffor || (acos (cos cliffor) ≈≈ 0.5 * (pi - 2 * asin(cos cliffor)))
                                                else acos (cos cliffor) ≈≈ 0.5 * (pi - 2 * asin(cos cliffor)))

prop_SinhAsinh :: Cl3 -> Bool
prop_SinhAsinh (cliffor) = sinh (asinh cliffor) ≈≈ cliffor

prop_AsinhSinh :: Cl3 -> Bool
prop_AsinhSinh (cliffor) = (abs cliffor > 10) || (asinh (sinh cliffor) ≈≈ log (0.5*(exp cliffor - exp (negate cliffor)) +
                                                                                   sqrt (0.25 * (exp cliffor - exp (negate cliffor))^2 + 1)))

prop_CoshAcosh :: Cl3 -> Bool
prop_CoshAcosh (cliffor) = if hasNilpotent cliffor
                           then poles [R 1, R (-1)] cliffor || (cosh (acosh cliffor) ≈≈ cliffor)
                           else cosh (acosh cliffor) ≈≈ cliffor

prop_AcoshCosh :: Cl3 -> Bool
prop_AcoshCosh (cliffor) = acosh (cosh cliffor) ≈≈ log (0.5*(exp cliffor + exp (negate cliffor)) +
                                                        sqrt (0.5*(exp cliffor + exp (negate cliffor)) - 1) *
                                                        sqrt (0.5*(exp cliffor + exp (negate cliffor)) + 1))

prop_AcoshCosh2 :: Cl3 -> Bool
prop_AcoshCosh2 (cliffor) = acosh (cosh cliffor) ≈≈ log (cosh cliffor + sqrt (cosh cliffor - 1) * sqrt (cosh cliffor + 1))

prop_DubSin :: Cl3 -> Bool
prop_DubSin (cliffor) = sin (2 * cliffor) ≈≈ 2 * sin cliffor * cos cliffor

prop_DubCos :: Cl3 -> Bool
prop_DubCos (cliffor) = cos (2 * cliffor) ≈≈ cos cliffor ^ 2 - sin cliffor ^ 2

prop_DubTan :: Cl3 -> Bool
prop_DubTan (cliffor) = poles [R (-pi), R (-3*pi/4), R (-pi/2), R (-pi/4), R (pi/4), R (pi/2), R (3*pi/4), R (pi)] cliffor ||
                        (tan (2 * cliffor) ≈≈ (2 * tan cliffor) / (1 - tan cliffor ^ 2))

prop_DubSinh :: Cl3 -> Bool
prop_DubSinh (cliffor) = sinh (2 * cliffor) ≈≈ 2 * sinh cliffor * cosh cliffor

prop_DubCosh :: Cl3 -> Bool
prop_DubCosh (cliffor) = cosh (2 * cliffor) ≈≈ 2 * cosh cliffor ^ 2 - 1

-- The test has poles at imaginary eigenvalues of n*pi/4 even is poles in the denominator and odd is poles in the numerator
-- The poles are a source of a loss of precision.
prop_DubTanh :: Cl3 -> Bool
prop_DubTanh (cliffor) = poles [I (-pi), I (-3*pi/4), I (-pi/2), I (-pi/4), I (pi/4), I (pi/2), I (3*pi/4), I (pi)] cliffor ||
                         (tanh (2 * cliffor) ≈≈ (2 * tanh cliffor) / (1 + tanh cliffor ^ 2))

prop_PosSinShift :: Cl3 -> Bool
prop_PosSinShift (cliffor) = sin (pi/2 + cliffor) ≈≈ cos cliffor

prop_NegSinShift :: Cl3 -> Bool
prop_NegSinShift (cliffor) = sin (pi/2 - cliffor) ≈≈ cos cliffor

prop_SinSqCosSq :: Cl3 -> Bool
prop_SinSqCosSq (cliffor) = (abs cliffor > 10) || (sin cliffor ^ 2 + cos cliffor ^ 2 ≈≈ 1)

prop_CoshSqmSinhSq :: Cl3 -> Bool
prop_CoshSqmSinhSq (cliffor) = (abs cliffor > 10) || (cosh cliffor ^ 2 - sinh cliffor ^ 2 ≈≈ 1)

prop_SymCosh :: Cl3 -> Bool
prop_SymCosh (cliffor) = cosh (negate cliffor) ≈≈ cosh cliffor

prop_SymSinh :: Cl3 -> Bool
prop_SymSinh (cliffor) = sinh (negate cliffor) ≈≈ negate (sinh cliffor)

prop_DoubleISin :: Cl3 -> Bool
prop_DoubleISin (cliffor) = 2 * I 1 * sin cliffor ≈≈ exp(I 1 * cliffor) - exp (mIx cliffor)

-- | Composition Sub-Algebras have a distributive norm over multiplication,
-- like this:
-- 
-- > norm $ clif * clif' = norm clif * norm clif'
--
-- Strangly the constructor combinations with the "= True" don't play nice
-- with 'abs' they are the constructors with non-zero zero-divisors.
prop_CompAlg :: (Cl3, Cl3) -> Bool
prop_CompAlg (PV{}, PV{}) = True
prop_CompAlg (PV{}, BPV{}) = True
prop_CompAlg (PV{}, TPV{}) = True
prop_CompAlg (PV{}, APS{}) = True
prop_CompAlg (BPV{}, PV{}) = True
prop_CompAlg (TPV{}, PV{}) = True
prop_CompAlg (APS{}, PV{}) = True
prop_CompAlg (BPV{}, BPV{}) = True
prop_CompAlg (BPV{}, TPV{}) = True
prop_CompAlg (BPV{}, APS{}) = True
prop_CompAlg (TPV{}, BPV{}) = True
prop_CompAlg (APS{}, BPV{}) = True
prop_CompAlg (TPV{}, TPV{}) = True
prop_CompAlg (TPV{}, APS{}) = True
prop_CompAlg (APS{}, TPV{}) = True
prop_CompAlg (APS{}, APS{}) = True
prop_CompAlg (cliffor, cliffor') = abs ( cliffor * cliffor') ≈≈ abs cliffor * abs cliffor'

----------------------------------------------------
-- Helper functions for the properties
----------------------------------------------------

-- | '≈≈' aproximately equal, using a mean squared error like calculation
-- across the 8 dimensional vector space of APS.  The properties are 
-- equivelent symbolicly but differ due to numerical errors.
(≈≈) :: Cl3 -> Cl3 -> Bool
(toAPS -> (APS a0 a1 a2 a3 a23 a31 a12 a123)) ≈≈ (toAPS -> (APS b0 b1 b2 b3 b23 b31 b12 b123)) =
  let m0 = (a0 - b0)^2
      m1 = (a1 - b1)^2
      m2 = (a2 - b2)^2
      m3 = (a3 - b3)^2
      m23 = (a23 - b23)^2
      m31 = (a31 - b31)^2
      m12 = (a12 - b12)^2
      m123 = (a123 - b123)^2
      sumsq = m0 + m1 + m2 + m3 + m23 + m31 + m12 + m123
      var = sumsq / 8
  in var <= 2e-13
_ ≈≈ _ = error "Everything passed to (≈≈) should be caught by toAPS/APS pattern match"
infix 4 ≈≈

-- | 'poles' a function that tests if a cliffor is one of the defined poles
poles :: [Cl3] -> Cl3 -> Bool
poles [] _ = False
poles [p] cliffor = eig1 `closeTo` p || eig2 `closeTo` p
  where (eig1,eig2) = eigvals cliffor
poles (p:ps) cliffor = (eig1 `closeTo` p || eig2 `closeTo` p) || poles ps cliffor
  where (eig1,eig2) = eigvals cliffor

-- | 'closeTo' used with poles to determine if an eigenvalue is close to a pole
-- the current threshold is 1e-3
closeTo :: Cl3 -> Cl3 -> Bool
closeTo (toC -> (C a0 a123)) (toC -> (C b0 b123)) =
  let diffR = abs (a0 - b0)
      diffI = abs (a123 - b123)
      magDiff = sqrt (diffR^2 + diffI^2)
  in magDiff < 2e-3
closeTo _ _ = error "Everything passed to 'closeTo' should be caught by toC/C pattern match"

-- | 'unWrapIPartEigs' a function to reduce the magnitude of the imaginary
-- portion of the Eigenvalues
unWrapIPartEigs :: Cl3 -> Cl3
unWrapIPartEigs cliffor = reduce $ spectraldcmp unWrapI id cliffor
  where unWrapI (R a0) = R a0
        unWrapI (I a123) | a123 > pi = unWrapI $ I (a123 - 2*pi)
                         | a123 < (-pi) = unWrapI $ I (a123 + 2*pi)
                         | otherwise = I a123
        unWrapI (C a0 a123) | a123 > pi = unWrapI $ C a0 (a123 - 2*pi)
                            | a123 < (-pi) = unWrapI $ C a0 (a123 + 2*pi)
                            | otherwise = C a0 a123
        unWrapI _ = error "unWrapI should only be unWrapping R I and C"

-- End of File
