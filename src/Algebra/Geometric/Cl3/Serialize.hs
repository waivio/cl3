{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=130 #-}

-------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2017 Nathan Waivio
-- License     :  BSD3
-- Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
-- Stability   :  Stable
-- Portability :  unportable 
-- 
-- Serialization of Cl3 types with the "Data.Serialize" library. 
-- 
-- >>> ghc -XSafe Algebra/Geometric/Cl3/Serialize.hs
-- [1 of 2] Compiling Algebra.Geometric.Cl3 ( Algebra/Geometric/Cl3.hs, Algebra/Geometric/Cl3.o )
-- [2 of 2] Compiling Algebra.Geometric.Cl3.Serialize ( Algebra/Geometric/Cl3/Serialize.hs, Algebra/Geometric/Cl3/Serialize.o )
-- Algebra/Geometric/Cl3/Serialize.hs:27:1: error:
--      Data.Serialize: Can't be safely imported!
--      The module itself isn't safe.
-- 
-- Serialize orphan instance fails to compile with -O2 because it runs out of simplifier ticks.
-- 
-- This module is Unsafe because 'Data.Serialize' is Unsafe.  
-- Use the "Data.Binary" instance of Cl3 instead for safe serialization.
--
-------------------------------------------------------------------


module Algebra.Geometric.Cl3.Serialize () where

import Algebra.Geometric.Cl3 (Cl3(..))
import Data.Serialize (Serialize)

-- | Uses 'GHC.Generics' to automatically derive a 'Serialize' instance
instance Serialize Cl3

