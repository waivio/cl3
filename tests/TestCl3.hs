{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


-------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2017 Nathan Waivio
-- License     :  BSD3
-- Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
-- 
-- A program to test Algebra.Geometric.Cl3
-- The code runs tests on some standard test input and then
-- runs quckcheck for some trig identities.
-- 
-------------------------------------------------------------------

module Main (main) where

import Test.QuickCheck (Arbitrary, arbitrary, oneof, suchThat, quickCheckWith, stdArgs, maxSuccess)
import Algebra.Geometric.Cl3
import Control.Applicative ((<*>), (<$>))


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
-- * The implementation of Arbitrary for Cl3 limits the arbitrary cliffor such that the absolute value of cliff is less than 15
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
main = do moduleTests
          print "Testing log.exp Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_LogExp
          print "Testing exp.log Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_ExpLog
          print "Testing abs*signum law:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_AbsSignum
          print "Testing the definition of recip:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_RecipDef
          print "Testing recip.recip Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_RecipID
          print "Testing sin.asin Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_SinAsin
          print "Testing asin.sin Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_AsinSin
          print "Testing cos.acos Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_CosAcos
          print "Testing acos.cos Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_AcosCos
          print "Testing sinh.asinh Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_SinhAsinh
          print "Testing asinh.sinh Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_AsinhSinh
          print "Testing cosh.acosh Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_CoshAcosh
          print "Testing acosh.cosh Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_AcoshCosh
          print "Testing acosh.cosh Identity2:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_AcoshCosh2
          print "Testing Double Sin Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_DubSin
          print "Testing Double Cos Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_DubCos
          print "Testing Double Tan Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_DubTan
          print "Testing Double Sinh Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_DubSinh
          print "Testing Double Cosh Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_DubCosh
          print "Testing Double Tanh Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_DubTanh
          print "Testing Positive Sin Shift Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_PosSinShift
          print "Testing Negative Sin Shift Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_NegSinShift
          print "Testing sin^2+cos^2 Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_SinSqCosSq
          print "Testing cosh^2-sinh^2 Identity:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_CoshSqmSinhSq
          print "Testing Symmetry of Cosh:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_SymCosh
          print "Testing Symmetry of Sinh:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_SymSinh
          print "Testing Double I Sin:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_DoubleISin
          print "Is has Composition Sub-Algebras:"
          quickCheckWith stdArgs { maxSuccess = 30000 } prop_CompAlg



----------------------------------------------------------
-- |Start of Module Tests
moduleTests :: IO ()
moduleTests = sequence_ $ tests <*> inputs

inputs :: [Cl3]
inputs = [R 0
         ,APS 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8
         ,PV 0.5 0 0 0.5
         ,PV 0.5 0 0 (-0.5)
         ,BPV 0.5 0 0 0 (-0.5) 0
         ,BPV 0.5 0 0 0 0.5 0
         ,R 1
         ,V3 1 0 0
         ,APS 1 0.5 0 0 0 0.5 0 0
         ,APS 1 0.5 0 0 0 (-0.5) 0 0
         ,PV 1 1 0 0
         ,R 1
         ,R (-1)
         ,V3 1 0 0
         ,V3 (-1) 0 0
         ,V3 0 1 0
         ,V3 0 (-1) 0
         ,V3 0 0 1
         ,V3 0 0 (-1)
         ,BV 1 0 0
         ,BV (-1) 0 0
         ,BV 0 1 0
         ,BV 0 (-1) 0
         ,BV 0 0 1
         ,BV 0 0 (-1)
         ,I 1
         ,I (-1)
         ]

-- | 'tests' is a list of tests
-- The out of bounds can be the poles of the function or if the cliffor has
-- nilpotent content then the poles of the derivative as well as the function
tests :: [Cl3 -> IO()]
tests = [runTest "Log.Exp Identity" (log.exp) id (const False)
        ,runTest "Exp.Log Identity" (exp.log) id (\z -> lsv z < tol) -- singular inputs are out of bounds
        ,runTest "Abs*Signum Identity" (\x->abs x * signum x) id (const False)
        ,runTest "Reciprical Identity" (recip.recip) id (\z -> lsv z < tol) -- singular inputs are out of bounds
        ,runTest "sin.asin" (sin.asin) id (\z -> hasNilpotent z && poles [R 1, R (-1)] z)
        ,runTest "asin.sin" (asin.sin) (\z -> negate (I 1) * log (0.5 * (exp (I 1 * z) - exp (I (-1) * z)) +
                                                                  sqrt (1+0.25*(exp (I (-1) * z) - exp (I 1 * z))^2))) (const False)
        ,runTest "cos.acos" (cos.acos) id (\z -> hasNilpotent z && poles [R 1, R (-1)] z)
        ,runTest "acos.cos" (acos.cos) (\z -> 0.5 * (pi - 2 * asin(cos z))) (\z -> hasNilpotent z && poles [R 0, pi, negate pi] z)
        ,runTest "sinh.asinh" (sinh.asinh) id (const False)
        ,runTest "asinh.sinh" (asinh.sinh) (\z -> log (0.5*(exp z - exp (negate z)) + sqrt (0.25 * (exp z - exp (negate z))^2 + 1))) (const False)
        ,runTest "cosh.acosh" (cosh.acosh) id (\z -> hasNilpotent z && poles [R 1, R (-1)] z)
        ,runTest "acosh.cosh" (acosh.cosh) (\z -> log (0.5*(exp z + exp (negate z)) +
                                                 sqrt (0.5*(exp z + exp (negate z)) - 1) * sqrt (0.5*(exp z + exp (negate z)) + 1))) (const False)
        ,runTest "Double Angle sin" (\z -> sin (2 * z)) (\z -> 2 * sin z * cos z) (const False)
        ,runTest "Double Angle cos" (\z -> cos (2 * z)) (\z -> cos z ^ 2 - sin z ^ 2) (const False)
        ,runTest "Double Angle tan" (\z -> tan (2 * z)) (\z -> (2 * tan z) / (1 - tan z ^ 2)) (const False)
        ,runTest "+Sin Shift" (\z -> sin (pi/2 + z)) cos (const False)
        ,runTest "-Sin Shift" (\z -> sin (pi/2 - z)) cos (const False)
        ,runTest "Double Angle sinh" (\z -> sinh (2 * z)) (\z -> 2 * sinh z * cosh z) (const False)
        ,runTest "Double Angle cosh" (\z -> cosh (2 * z)) (\z -> 2 * cosh z ^ 2 - 1) (const False)
        ,runTest "Double Angle tanh" (\z -> tanh (2 * z)) (\z -> (2 * tanh z) / (1 + tanh z ^ 2)) (const False)
        ,runTest "sin^2+cos^2" (\z -> sin z ^ 2 + cos z ^ 2) (const $ R 1) (const False)
        ,runTest "cosh^2-sinh^2" (\z -> cosh z ^ 2 - sinh z ^ 2) (const $ R 1) (const False)
        ,runTest "Symetry of cosh" (cosh.negate) cosh (const False)
        ,runTest "Symetry of sinh" (sinh.negate) (negate.sinh) (const False)
        ,runTest "sin.acos" (sin.acos) (\z -> sqrt (1 - z^2)) (\z -> hasNilpotent z && poles [R 1, R (-1)] z)
        ,runTest "sin.atan" (sin.atan) (\z -> z / sqrt (1 + z^2)) (poles [I 1, I (-1)])
        ,runTest "cos.atan" (cos.atan) (\z -> recip.sqrt $ 1 + z^2) (poles [I 1, I (-1)])
        ,runTest "cos.asin" (cos.asin) (\z -> sqrt (1 - z^2)) (\z -> hasNilpotent z && poles [R 1, R (-1)] z)
        ,runTest "tan.asin" (tan.asin) (\z -> z / sqrt (1 - z^2)) (poles [R 1, R (-1)])
        ,runTest "tan.acos" (tan.acos) (\z -> sqrt (1 - z^2) / z) (\z -> if hasNilpotent z then poles [R 1, R 0, R (-1)] z else poles [R 0] z)
        ]

-- | The Properties
prop_LogExp :: ArbCl3 -> Bool
prop_LogExp (Arb cliffor) = (abs cliffor > 10) || (
  let cliffor' = unWrapIPartEigs cliffor  -- imaginary part of log.exp repeats
-- round off errors get large for exp larger than 5 use spectproj (log.exp) for accuracy
  in log (exp cliffor') ≈≈ cliffor')

-- log 0 is -Inf, Infinite vectors don't play nice
-- spectproj (exp.log) doesn't have this issue
prop_ExpLog :: ArbCl3 -> Bool
prop_ExpLog (Arb cliffor) = (lsv cliffor < tol) || (exp (log cliffor) ≈≈ cliffor)

prop_AbsSignum :: ArbCl3 -> Bool
prop_AbsSignum (Arb cliffor) = abs cliffor * signum cliffor ≈≈ cliffor

prop_RecipDef :: ArbCl3 -> Bool
prop_RecipDef (Arb cliffor) = (lsv cliffor < tol) || (recip cliffor * cliffor ≈≈ 1)

-- singular inputs don't recip also suffers from roundoff errors at large values
prop_RecipID :: ArbCl3 -> Bool
prop_RecipID (Arb cliffor) = (lsv cliffor < tol) || (recip (recip cliffor) ≈≈ cliffor)

prop_SinAsin :: ArbCl3 -> Bool
prop_SinAsin (Arb cliffor) = if hasNilpotent cliffor
                             then poles [R 1, R (-1)] cliffor || (sin (asin cliffor) ≈≈ cliffor)
                             else sin (asin cliffor) ≈≈ cliffor

prop_AsinSin :: ArbCl3 -> Bool
prop_AsinSin (Arb cliffor) = (abs cliffor > 10) || (asin (sin cliffor) ≈≈ (I (-1) * log (0.5 * (exp (I 1 * cliffor) - exp (I (-1) * cliffor)) +
                                                                                         sqrt (1+0.25*(exp (I (-1) * cliffor) - exp (I 1 * cliffor))^2))))

prop_CosAcos :: ArbCl3 -> Bool
prop_CosAcos (Arb cliffor) = if hasNilpotent cliffor
                             then poles [R 1, R (-1)] cliffor || (cos (acos cliffor) ≈≈ cliffor)
                             else cos (acos cliffor) ≈≈ cliffor

prop_AcosCos :: ArbCl3 -> Bool
prop_AcosCos (Arb cliffor) = (abs cliffor > 10) || (if hasNilpotent cliffor
                                                    then poles [R 0, pi, negate pi] cliffor || (acos (cos cliffor) ≈≈ 0.5 * (pi - 2 * asin(cos cliffor)))
                                                    else acos (cos cliffor) ≈≈ 0.5 * (pi - 2 * asin(cos cliffor)))

prop_SinhAsinh :: ArbCl3 -> Bool
prop_SinhAsinh (Arb cliffor) = sinh (asinh cliffor) ≈≈ cliffor

prop_AsinhSinh :: ArbCl3 -> Bool
prop_AsinhSinh (Arb cliffor) = (abs cliffor > 10) || (asinh (sinh cliffor) ≈≈ log (0.5*(exp cliffor - exp (negate cliffor)) +
                                                                                   sqrt (0.25 * (exp cliffor - exp (negate cliffor))^2 + 1)))

prop_CoshAcosh :: ArbCl3 -> Bool
prop_CoshAcosh (Arb cliffor) = if hasNilpotent cliffor
                               then poles [R 1, R (-1)] cliffor || (cosh (acosh cliffor) ≈≈ cliffor)
                               else cosh (acosh cliffor) ≈≈ cliffor

prop_AcoshCosh :: ArbCl3 -> Bool
prop_AcoshCosh (Arb cliffor) = acosh (cosh cliffor) ≈≈ log (0.5*(exp cliffor + exp (negate cliffor)) +
                                                            sqrt (0.5*(exp cliffor + exp (negate cliffor)) - 1) *
                                                            sqrt (0.5*(exp cliffor + exp (negate cliffor)) + 1))

prop_AcoshCosh2 :: ArbCl3 -> Bool
prop_AcoshCosh2 (Arb cliffor) = acosh (cosh cliffor) ≈≈ log (cosh cliffor + sqrt (cosh cliffor - 1) * sqrt (cosh cliffor + 1))

prop_DubSin :: ArbCl3 -> Bool
prop_DubSin (Arb cliffor) = sin (2 * cliffor) ≈≈ 2 * sin cliffor * cos cliffor

prop_DubCos :: ArbCl3 -> Bool
prop_DubCos (Arb cliffor) = cos (2 * cliffor) ≈≈ cos cliffor ^ 2 - sin cliffor ^ 2

prop_DubTan :: ArbCl3 -> Bool
prop_DubTan (Arb cliffor) = tan (2 * cliffor) ≈≈ (2 * tan cliffor) / (1 - tan cliffor ^ 2)

prop_DubSinh :: ArbCl3 -> Bool
prop_DubSinh (Arb cliffor) = sinh (2 * cliffor) ≈≈ 2 * sinh cliffor * cosh cliffor

prop_DubCosh :: ArbCl3 -> Bool
prop_DubCosh (Arb cliffor) = cosh (2 * cliffor) ≈≈ 2 * cosh cliffor ^ 2 - 1

prop_DubTanh :: ArbCl3 -> Bool
prop_DubTanh (Arb cliffor) = tanh (2 * cliffor) ≈≈ (2 * tanh cliffor) / (1 + tanh cliffor ^ 2)

prop_PosSinShift :: ArbCl3 -> Bool
prop_PosSinShift (Arb cliffor) = sin (pi/2 + cliffor) ≈≈ cos cliffor

prop_NegSinShift :: ArbCl3 -> Bool
prop_NegSinShift (Arb cliffor) = sin (pi/2 - cliffor) ≈≈ cos cliffor

prop_SinSqCosSq :: ArbCl3 -> Bool
prop_SinSqCosSq (Arb cliffor) = (abs cliffor > 10) || (sin cliffor ^ 2 + cos cliffor ^ 2 ≈≈ 1)

prop_CoshSqmSinhSq :: ArbCl3 -> Bool
prop_CoshSqmSinhSq (Arb cliffor) = (abs cliffor > 10) || (cosh cliffor ^ 2 - sinh cliffor ^ 2 ≈≈ 1)

prop_SymCosh :: ArbCl3 -> Bool
prop_SymCosh (Arb cliffor) = cosh (negate cliffor) ≈≈ cosh cliffor

prop_SymSinh :: ArbCl3 -> Bool
prop_SymSinh (Arb cliffor) = sinh (negate cliffor) ≈≈ negate (sinh cliffor)

prop_DoubleISin :: ArbCl3 -> Bool
prop_DoubleISin (Arb cliffor) = 2 * I 1 * sin cliffor ≈≈ exp(I 1 * cliffor) - exp (I (-1) * cliffor)

-- | Composition Sub-Algebras have a distributive norm over multiplication, like this:
-- 
-- > norm $ clif * clif' = norm clif * norm clif'
--
-- Strangly the constructor combinations with the "= True" don't play nice with 'abs'
-- they are the constructors with non-zero zero-divisors.
prop_CompAlg :: (ArbCl3, ArbCl3) -> Bool
prop_CompAlg (Arb PV{}, Arb PV{}) = True
prop_CompAlg (Arb PV{}, Arb BPV{}) = True
prop_CompAlg (Arb PV{}, Arb TPV{}) = True
prop_CompAlg (Arb PV{}, Arb APS{}) = True
prop_CompAlg (Arb BPV{}, Arb PV{}) = True
prop_CompAlg (Arb TPV{}, Arb PV{}) = True
prop_CompAlg (Arb APS{}, Arb PV{}) = True
prop_CompAlg (Arb BPV{}, Arb BPV{}) = True
prop_CompAlg (Arb BPV{}, Arb TPV{}) = True
prop_CompAlg (Arb BPV{}, Arb APS{}) = True
prop_CompAlg (Arb TPV{}, Arb BPV{}) = True
prop_CompAlg (Arb APS{}, Arb BPV{}) = True
prop_CompAlg (Arb TPV{}, Arb TPV{}) = True
prop_CompAlg (Arb TPV{}, Arb APS{}) = True
prop_CompAlg (Arb APS{}, Arb TPV{}) = True
prop_CompAlg (Arb APS{}, Arb APS{}) = True
prop_CompAlg (Arb cliffor, Arb cliffor') = abs ( cliffor * cliffor') ≈≈ abs cliffor * abs cliffor'


-- Run the test
-- compare the function under test (fUT) to a golden test funcion (gTF)
-- if the input is within bounds
runTest :: String -> (Cl3 -> Cl3) -> (Cl3 -> Cl3) -> (Cl3 -> Bool) -> Cl3 -> IO()
runTest testName fUT gTF outOB iVal =
  let f = fUT iVal
      g = gTF iVal
  in if outOB iVal
       then putStr (unlines [testName ++ ": Input Out of Bounds"])
       else if f ≈≈ g
              then putStr (unlines [testName ++ ": Passed"])
              else putStr (unlines [testName ++ ": Failed"
                                   ,"Expected: " ++ show g
                                   ,"     got: " ++ show f
                                   ,"on input: " ++ show iVal
                                   ])

-- | '≈≈' aproximately equal
(≈≈) :: Cl3 -> Cl3 -> Bool
(reduce -> clifforA) ≈≈ (reduce -> clifforB) =
  let ave = (abs clifforA + abs clifforB) / 2
  in abs (clifforA - clifforB) <= 1e-5*ave + tol
infix 4 ≈≈

-- | 'poles' a function that tests if a cliffor is one of the defined poles
poles :: [Cl3] -> Cl3 -> Bool
poles [] _ = False
poles [p] cliffor = eig1 ≈≈ p || eig2 ≈≈ p
  where (eig1,eig2) = eigvals cliffor
poles (p:ps) cliffor = (eig1 ≈≈ p || eig2 ≈≈ p) || poles ps cliffor
  where (eig1,eig2) = eigvals cliffor

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

----------------------------------------------------------

-------------------------------------------------------------------
-- 
-- Arbitrary Instance of Cl3 types, typically for use with the 
-- "Test.QuickCheck" library. 
-- 
-------------------------------------------------------------------

-- | 'ArbCl3' to provide a newtype wrapper to avoid the orphan instance
newtype ArbCl3 = Arb Cl3 deriving (Show)

-- | 'Arbitrary' instance that has its largest singular value less than or equal to 15
instance Arbitrary ArbCl3 where
  arbitrary = 
     oneof [(Arb.)R <$> arbitrary, 
            ((Arb.).).V3 <$> arbitrary <*> arbitrary <*> arbitrary,
            ((Arb.).).BV <$> arbitrary <*> arbitrary <*> arbitrary,
            (Arb.)I <$> arbitrary,
            (((Arb.).).).PV <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
            (((Arb.).).).H <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
            (Arb.).C <$> arbitrary <*> arbitrary,
            (((((Arb.).).).).).BPV <$> arbitrary <*> arbitrary <*> arbitrary 
                                   <*> arbitrary <*> arbitrary <*> arbitrary,
            (((Arb.).).).ODD <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
            (((Arb.).).).TPV <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
            (((((((Arb.).).).).).).).APS <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary 
                                         <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
            ] `suchThat` lessThan15
    where
      lessThan15 (Arb cliffor) = abs cliffor <= 15
