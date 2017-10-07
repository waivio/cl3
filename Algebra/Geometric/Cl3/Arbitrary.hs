{-# LANGUAGE Safe #-}

-------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2017 Nathan Waivio
-- License     :  BSD3
-- Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
-- Stability   :  Stable
-- Portability :  unportable
-- 
-- Arbitrary Instance of Cl3 types, typically for use with the 
-- "Test.QuickCheck" library. 
-- 
-------------------------------------------------------------------

module Algebra.Geometric.Cl3.Arbitrary () where

import Algebra.Geometric.Cl3 (Cl3(..))
import Test.QuickCheck (Arbitrary, arbitrary, oneof, suchThat)

-- | 'Arbitrary' instance that has its largest singular value less than or equal to 15
instance Arbitrary Cl3 where
  arbitrary = 
    (oneof [R <$> arbitrary, 
            V3 <$> arbitrary <*> arbitrary <*> arbitrary,
            BV <$> arbitrary <*> arbitrary <*> arbitrary,
            I <$> arbitrary,
            PV <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
            H <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
            C <$> arbitrary <*> arbitrary,
            BPV <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
            ODD <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
            TPV <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
            APS <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
            ]) `suchThat` lessThan15

lessThan15 cliffor = abs cliffor <= 15

