{-# LANGUAGE Safe #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2017 Nathan Waivio
-- License     :  BSD3
-- Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
-- Stability   :  Stable
-- Portability :  unportable
-- 
-- Instance of Cl3 types with the "Foreign.Storable" library.
--  
-- For use with high performance data structures like Data.Vector.Storable
-- or Data.Array.Storable
-- 
-------------------------------------------------------------------


module Algebra.Geometric.Cl3.Storable () where

import Algebra.Geometric.Cl3 (Cl3(..),toAPS)
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke)
import Foreign.Ptr (Ptr, plusPtr, castPtr)

-- | Cl3 instance of Storable uses the APS constructor as its standard interface.
-- "peek" returns a cliffor constructed with APS. "poke" converts a cliffor to APS.
instance Storable Cl3 where
  sizeOf _ = 8 * dubbSize
  alignment _ = dubbSize
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

dubbSize :: Int
dubbSize = sizeOf (undefined :: Double)

