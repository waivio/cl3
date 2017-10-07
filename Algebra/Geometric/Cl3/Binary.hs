{-# LANGUAGE Safe #-}

-------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2017 Nathan Waivio
-- License     :  BSD3
-- Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
-- Stability   :  Stable
-- Portability :  unportable
-- 
-- Serialization of Cl3 types with the "Data.Binary" library.
-- 
-------------------------------------------------------------------

module Algebra.Geometric.Cl3.Binary () where

import Algebra.Geometric.Cl3 (Cl3(..))
import Data.Binary (Binary)

-- | Uses 'GHC.Generics' to automatically derive a 'Data.Binary' instance
instance Binary Cl3

