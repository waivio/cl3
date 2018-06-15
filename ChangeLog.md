# Revision history for cl3

## 1.0.0.0  -- 2017-10-28

* First version. Released on an unsuspecting world.

## 1.0.0.1  -- 2018-06-10

* Used Stack to test different versions of GHC.
* Removed {-# OPTIONS_GHC -fno-warn-unused-top-binds #-} from Cl3.hs to better support earlier versions of GHC, and it was no longer needed.
* Loosened version bound for QuickCheck to work better with earlier versions of Stackage LTS snapshots.
* Improved spectraldcmp's documentation to clairify that spectraldcmp requires an implementation of the real, imaginary, and complex implememtation of the function.

